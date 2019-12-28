var page = require('webpage').create();
page.open('http://www.scoreboard.com/game/rosol-l-goffin-d-2014/8drhX07d/#game-summary', function () {
    console.log(page.content); //page source
    phantom.exit();
});
