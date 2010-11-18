NB.CreateNoteList = function(el)
{
    var _notebookEl = $(el);
    var _createButton = _notebookEl.find('.create-button');
    var _listEl = _notebookEl.find('.note-list');
    
    var _selectedId = null;
    var _selectedNote = null;
    var _noteList = null;
    var _precisHolder = null;

    
    var _createNotePrecis = function(noteData) {
        var precisEl = document.createElement("div");
        precisEl.setAttribute("class", "note-precis");
            
        precisEl.setAttribute("data-noteid", noteData["_id"]);


        // Server guarantees that the title attribute will be present        
        var titleEl = document.createElement("div");
        titleEl.appendChild(document.createTextNode(noteData["title"]));
        titleEl.setAttribute("class", "note-title");
 
        
        // Server guarantees that the content attribute will be present
        var contentEl = document.createElement("div");
        contentEl.appendChild(document.createTextNode(noteData["content"]));
        contentEl.setAttribute("class", "note-firstline");
        
        precisEl.appendChild(titleEl);
        precisEl.appendChild(contentEl);

        
        return precisEl;
    };

    
    
    var _renderNoteList = function(selectedId) {
        _listEl.empty();

        _precisHolder = document.createElement("div");
        _precisHolder.setAttribute("class", "precis-holder");

        var notes = _noteList["rows"];
    
        var precis = null;
        var curr = null;
        var result = null;

        if (notes.length === 0)
        {
            _createNote();
        }
        else
        {
            for (var ctr = 0; ctr < notes.length; ++ctr)
            {
                curr = notes[ctr].value;
                if (curr) {
                    precis = _createNotePrecis(curr);
                    if (curr._id === selectedId)
                        result = $(precis);

                    _precisHolder.appendChild(precis);
                }
            }
        }
        
        _listEl.append(_precisHolder);

        return result;
    };
    

    var _selectById = function(id, precis) {
        var oldSelection = _selectedId;
        _selectedId = _noteList["selected"] = id;

        var notes = _noteList["rows"];
        var l = notes.length;
        _selectedNote = null;
        for (var ctr = 0; ctr < l; ++ctr)
        {
            if (notes[ctr].id === _selectedId)
            {
                _selectedNote = notes[ctr].value;
                break;
            }
        }
        
        if (oldSelection !== _selectedId)
        {
            if (!precis)
                precis = _listEl.find("[data-noteid="+id+"]");
            
            _listEl.find('.selected').removeClass('selected');
            precis.addClass('selected');

            NB.App.NotificationCenter.publish("note selection changed", _selectedNote, this);
        }
    };


    var _createNote = function() {
        if (_precisHolder === null)
        {
            console.error("NoteList: no precis holder - cannot create note");
            return;
        }
        
        var id = NB.Utils.Uuid.create();
        var n = {
            _id: id,
            title: "",
            content: "",
            tags: [],
            authors: [],

            _rev: "new",
            created: "" + new Date(),
            modified: "" + new Date()
        };

        
        var precis = _createNotePrecis(n);
        precis.setAttribute("style", "display: none");

        _precisHolder.insertBefore(precis, _precisHolder.firstChild);

        _noteList["rows"].push({ id: id, key: "", value: n });

         precis = $(precis);

        _selectById(id, precis);
        
        $(precis).slideDown('fast');
    };


    
    var _netSuccess = function(data)
    {
        _noteList = data;

        var toSelect = null;
        if (_noteList.hasOwnProperty("selected"))
            toSelect = _noteList['selected'];
        else
            if (_noteList.hasOwnProperty('rows') && _noteList['rows'].length > 0)
                toSelect = _noteList['rows'][0].id;
        
        var precisToSelect = _renderNoteList(toSelect);

        if (toSelect !== null)
            _selectById(toSelect, precisToSelect);
        else
            console.warn("NoteList: Could not find an ID to select on net update");
    };
    
    
    var _netFailure = function(req, status, exception)
    {
        NB.App.Status.ShowWarning("Network problem: Could not get note list ("+status+")");
    };
    

    var _update = function()
    {
        _listEl.text("Loading...");
    
        $.ajax({
            type: 'GET',
            url: 'notes/',
            dataType: 'json',
            success: _netSuccess,
            error: _netFailure
        });
    };

    
    var _accChangeListener = function(accountData, source)
    {
        _update();
    };

    var _changeSelection = function(e) {
        var id = $(this).attr("data-noteid");
        if (!id)
        {
            console.error("NoteList: Could not find note ID on precis - cannot change selection");
            return;
        }
        
        _selectById(id, $(this));
    };



    var _getPrecisFor = function(noteId) {
        if (_precisHolder === null)
            return null;

        return $(_precisHolder).find(".note-precis[data-noteid="+noteId+"]");
    };


    // We have been notified that 'noteId' has been deleted - remove 
    // it from our list...
    var _deletionListener = function(noteId) {
        var precis = _getPrecisFor(noteId);

        var idx = 0;
        var l = _noteList.rows.length;
        for (idx = 0; idx < l; ++idx)
            if (_noteList.rows[idx].id === noteId)
                break;


        if (precis !== null && idx < l)
        {
            _noteList.rows.splice(idx, 1);
            precis.remove();

            if (_noteList.rows.length > 0)
            {
                if (idx === _noteList.rows.length)
                    idx -= 1;
                
                _selectById(_noteList.rows[idx].id);
            }
            else
            {
                _createNote();
            }
        }
    };

    var _replaceNote = function(note) {
        var rows = _noteList["rows"];
        for (var ctr = 0; ctr < rows.length; ++ctr) {
            if (rows[ctr].id === note._id) {
                rows[ctr].value = note;
                break;
            }
        }        
    };

    var _conflictResolutionListener = function(note) {
        var precis = _getPrecisFor(note._id);
        _replaceNote(note);

        precis.removeClass('conflicted').removeClass('unsaved');
    };


    var _synchronize = function() {
        var netSuccess = function(couchResult) {
            if (couchResult.hasOwnnProperty("ok") && couchResult.ok === true) {
                if (couchResult.hasOwnProperty("id")) {
                    // XXX: note could have been changed *again* before this
                    // result came back (given a slow enough connection).
                    // Should do something clever with versions...
                    var precis = _getPrecisFor(couchResult.id);
                    precis.removeClass('unsynced');
                }
            }
        };

        var netError = function(req, status, exn) {
            NB.App.ShowWarning('Network problem syncing note');
        };


        return (function() {
            var toSync = [];
            var rows = _noteList.rows;
            var l = rows.length;
            
            for (var ctr = 0; ctr < l; ++ctr) {
                if ((rows[ctr]._meta.syncd) && (rows[ctr]._meta.syncd === false)) {
                    toSync.push(rows[ctr].value);
                }
            }
            
            // TODO: Don't know whether to do a bulk save or save each
            // modified note individually - are large updates or many
            // small transactions worse??
            if (toSync.length > 0)
            {
                var callData = { notes: toSync };
                $.ajax({
                    type: 'POST',
                    url: 'notes/bulk_update/',
                    dataType: 'json',
                    success: netSuccess,
                    error: netError
                })
            }
        });
    };


    NB.App.NotificationCenter.subscribe("account changed", _accChangeListener);
    NB.App.NotificationCenter.subscribe('note deleted', _deletionListener);
    NB.App.NotificationCenter.subscribe('conflict resolved', _conflictResolutionListener);


    _listEl.find(".note-precis").live('click', _changeSelection);
    _createButton.click(_createNote);


    return {
        update: _update,
        getNoteList: function() { return _noteList; },
        getSelectedId: function() { return _selectedId; },
        getSelectedNote: function() { return _selectedNote; },
        getPrecisFor: _getPrecisFor,
        createNote: _createNote
    };
}