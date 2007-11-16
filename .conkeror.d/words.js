var minibuffer_word_separators = [' ', '/', '?'];

// TODO: 
//  * scroll input element if movement brings us out of visual range
//  * select, delete commands

// need to go back/forward while next char is not a word separator

function cmd_wordNext(prefix) {
    while(at_word_separator(0) && !at_buffer_limit(0)) {
	move_char();
    }
    while(! at_word_separator(0) && !at_buffer_limit(0)) {
	move_char();
    }
}
interactive("cmd_wordNext", cmd_wordNext, ["p"]);

function cmd_wordPrevious(prefix) { 
    while(at_word_separator(-1) && !at_buffer_limit(-1)) {
	move_char(-1);
    }
    while(! at_word_separator(-1) && !at_buffer_limit(-1)) {
	move_char(-1);
    }
}
interactive("cmd_wordPrevious", cmd_wordPrevious, ["p"]);

function at_word_separator(offset) {
    var value = document.commandDispatcher.focusedElement.value;
    var position = document.commandDispatcher.focusedElement.selectionEnd;
    var the_char =  value.substring(position + offset, position + offset + 1);
    
    return !! minibuffer_word_separators.indexOf(the_char);
}

function at_buffer_limit(offset) {
    var value = document.commandDispatcher.focusedElement.value;
    var position = document.commandDispatcher.focusedElement.selectionEnd;
    if((position >= value.length && offset >= 0) ||
       (position <= 0 && offset < 0)) {
	return true;
    }
}

function move_char(direction) {
    if(direction == undefined) direction = 1;
    var elt = document.commandDispatcher.focusedElement;
    var position = elt.selectionEnd + direction;

    elt.setSelectionRange(position, position);
}

// save the current position, go wordNext/Previous, select from old position to new

function cmd_selectWordNext(prefix) { doCommandNTimes(prefix,"cmd_selectWordNext"); }
interactive("cmd_selectWordNext", cmd_selectWordNext, ["p"]);

function cmd_selectWordPrevious(prefix) { doCommandNTimes(prefix,"cmd_selectWordPrevious"); }
interactive("cmd_selectWordPrevious", cmd_selectWordPrevious, ["p"]);

// select wordNext/Previous, then delete

function cmd_deleteWordBackward(prefix) { cmd_selectWordPrevious(); }
interactive("cmd_deleteWordBackward", cmd_deleteWordBackward, ["p"]);

function cmd_deleteWordForward(prefix) { doCommandNTimes(prefix,"cmd_deleteWordForward"); }
interactive("cmd_deleteWordForward", cmd_deleteWordForward, ["p"]);
