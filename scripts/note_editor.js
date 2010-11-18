NB.CreateNoteEditor = function(el) {
    var _noteEditorEl = $(el);
    var _mutators = _noteEditorEl.find('.note-input');

    var _title = _noteEditorEl.find('.note-title');
    var _content = _noteEditorEl.find('.note-content .note-text');

    var _tags = _noteEditorEl.find('.note-tags');
    var _tagList = _tags.find('.tag-list');
    var _addTag = _tags.find('.add-tag');
    var _tagInput = _tags.find('.note-tag-input');

    var _deletePanel = _noteEditorEl.find('.note-deleter');
    var _deleteButton = _deletePanel.find('.delete-button');
    var _deleteConfirm = _deletePanel.find('.confirm-button');
    var _deleteCancel = _deletePanel.find('.cancel-button');

    var _conflictBanner = _noteEditorEl.find('.conflict-banner');
    var _conflictReload = _conflictBanner.find('#reload_note');
    var _conflictMerge = _conflictBanner.find('#merge_conflict');


    var _noteData = null;

    var _autoSaver = null;
    
    var _precis = null;
    var _precisTitle = null;
    var _precisContent = null;


    var _makeTagElement = function(tagStr, idx)  {
        var html = [
            "<span class='note-tag displaying' data-tag-idx='"+idx+"'>",
            "<span class='label'>",
            tagStr,
            "</span>",
            "<div class='editor'>",
            "<input type='text' value='",
            tagStr,
            "' class='tag-editor' />",
            "<span class='deleter'>delete</span>",
            "</div>",
            "</span>"
        ].join("");
        _tagList.append(html);
    };


    var _setNote = function(noteData) {
        if (!noteData)
            return;
            
        if (!noteData.hasOwnProperty("_id"))
        {
            NB.App.Status.ShowWarning("Note supplied to editor does not have an ID");
            return;
        }
            
    
        if (_autoSaver)
            _autoSaver.cancelCurrent();
            
        _noteData = noteData;
        _title.val(_noteData["title"]);
        _content.val(_noteData["content"]);
        _tagList.empty();
        for (var c = 0; c < _noteData.tags.length; ++c)
            _makeTagElement(_noteData.tags[c], c);
        
        _precis = NB.App.NoteList.getPrecisFor(_noteData["_id"]);
        if (_precis != null && _precis.length > 0)
        {
            _precisTitle = _precis.find(".note-title");
            _precisContent = _precis.find(".note-firstline");
        }
        else
        {
            _precisTitle = null;
            _precisContent = null;
        }

        
        if (_autoSaver)
            _autoSaver.setPath("notes/"+_noteData["_id"]);
    };


    var _handleSaveError = function(couchResult)
    {
        var e = couchResult.error;
        result = false;
        if (e) {
            if (e === 'conflict') {
                _precis.addClass('conflicted');
                _conflictBanner.slideDown('fast');
                result = true;
            }
        }

        return result;
    }

    var _saveSuccess = function(couchResult) {
        if (couchResult.hasOwnProperty("ok") && (couchResult["ok"] === true))
        {
            if (couchResult.hasOwnProperty("rev")) 
            {
                _noteData["_rev"] = couchResult["rev"];
                _precis.removeClass('unsaved');
            }
            else
            {
                console.warn("Got success when saving note but no new revision number");
            }
        }
        else if (couchResult.hasOwnProperty("error")) 
        {
            if (!_handleSaveError(couchResult)) {
                if (couchResult.hasOwnProperty("reason"))
                    NB.App.Status.ShowWarning("Problem saving note: "+couchResult["reason"]);
                else
                    NB.App.Status.ShowWarning("Unknown problem saving note");
            }
        }
    };


    
    var _saveFailure = function(req, status, exception) {
        NB.App.Status.ShowWarning("Could not save note - could not contact server: " + status);

        
    };



    var _deleteNetSuccess = function(couchResult) {
        if (couchResult.hasOwnProperty("ok") && (couchResult.ok === true)) {
            _deletePanel.toggleClass('confirming').toggleClass('resting');
            NB.App.NotificationCenter.publish("note deleted", couchResult.id);
        } else {
            if (couchResult.hasOwnProperty("reason"))
                NB.App.Status.ShowWarning("Could not delete note: "+couchResult.reason);
            else
                NB.App.Status.ShowWarning('Could not delete note: '+couchResult.error);
        }
    };



    var _deleteNetFailure = function(req, status, exception) {
        NB.App.Status.ShowWarning('Network problem contacting server');
    };



    var _deleteNote = (function() {
        var callSpec = {
            type: 'POST',
            url: 'delete',
            data: { },
            dataType: 'json',
            success: _deleteNetSuccess,
            error: _deleteNetFailure
        };

        return function() {
            callSpec.data.id = _noteData._id;
            callSpec.data.rev = _noteData._rev;
            
            $.ajax(callSpec);
        }
    }());

    
    var _makeNoteData = function() {
        _noteData["title"] = _title.val();
        _noteData["content"] = _content.val();

        // Shalow copy note data...
        var result = jQuery.extend({}, _noteData);
        result["tags"] = _noteData.tags.join('\n');
        result["authors"] = _noteData.authors.join('\n');;

        result["selection"] = NB.App.NoteList.getSelectedId();

        return result;
    };
    

    var _precisUpdater = function(e) {
        _precisTitle.text(_title.val());
        _precisContent.text(_content.val());
        _precis.addClass('unsaved');
    };


    var _updateTags = function() {
        var newTag = _tagInput.val();
        if (newTag !== null && newTag !== "")
            _makeTagElement(newTag);

        _noteData["tags"].push(newTag);
        _autoSaver.queueSave();

        _tagInput.val("");
        _tagInput.blur();
    };


    var _autoSaver = NB.App.StorageManager.createDelayedSaver('', 1000, _makeNoteData, _saveSuccess, _saveFailure);
    _mutators.bind('keyup change', _autoSaver.queueSave);
    _mutators.bind('keyup change', _precisUpdater);


    _tagInput.bind('focus', function(e) { $(this).parent().css("opacity", "1"); });
    _tagInput.bind('blur', function(e) { $(this).parent().css('opacity', null); })
    _tagInput.bind('keyup', function(e) { if (e.keyCode === 13) { _updateTags(); } });
    _addTag.bind('click', _updateTags);
    _tagList.find('.note-tag .label').live(
        'click', 
        function(e) { 
            $(this).parent().toggleClass('editing').toggleClass('displaying').find('input').focus();
        });

    _tagList.find('.note-tag .tag-editor').live(
        'keyup',
        function(e) {
            var me = $(this);
            var p = me.parent().parent();;

            if (e.keyCode === 13) {
                var idx = p.data('tag-idx');
                if (idx !== null) {
                    _noteData.tags[idx] = me.val();
                    p.find('.label').text(jQuery.trim(me.val()));
                    p.toggleClass("editing").toggleClass('displaying');
                    _autoSaver.queueSave();
                } else {
                    console.warn("No idx found!");
                }
            } else if (e.keyCode === 27) {
                me.val(jQuery.trim(p.find('.label').text()));
                p.toggleClass("editing").toggleClass('displaying');
            }
        });

    _tagList.find('.note-tag .deleter').live(
        'click',
        function(e) {
            var p = $(this).parent().parent();
            var idx = p.data('tag-idx');
            if (idx !== null)
            {
                p.nextAll('.note-tag').each(function() {
                    var i = $(this).data('tag-idx');
                    if (i !== null) {
                        i -= 1;
                        $(this).data('tag-idx', ''+i);
                    }
                });
                
                p.remove();
                _noteData.tags.splice(idx, 1);
                _autoSaver.queueSave();
            }
        });


    _deleteButton.bind('click', function() {
        _deletePanel.toggleClass('resting').toggleClass('confirming');
    });

    _deleteCancel.bind('click', function() {
        _deletePanel.toggleClass('confirming').toggleClass('resting');
    });

    _deleteConfirm.bind('click', _deleteNote);
    

    _conflictReload.bind(
        'click', 
        (function() {
            var success = function(result) {
                _setNote(result);
                _conflictBanner.slideUp('fast');
                NB.App.NotificationCenter.publish('conflict resolved', result);
            };
        
            var error = function(req, status, excp) {
                NB.App.Status.ShowWarning("Could not reload note - network problem");
            };


            return function() {
                NB.App.StorageManager.read("notes/"+_noteData["_id"]+"/", success, error);
            }
        }())
    );


    NB.App.NotificationCenter.subscribe("note selection changed", _setNote);

    return {
        setNote: _setNote
    };
};