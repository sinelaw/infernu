var x = /hi/;

function matchesHi() {
    if (x.test('hi')) {
        return true;   
    }
    return false;
}

var m = matchesHi();

