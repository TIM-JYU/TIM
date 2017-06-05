
var leveys = 800;
var korkeus = 600;
var nopeus = 200;
var p1Pisteet = 0;
var p2Pisteet = 0;
var game = new Phaser.Game(leveys, korkeus, Phaser.CANVAS, 'pong', { preload: lataus, create: luonti, update: paivitys });
var tekstinTyyli = { font: "bold 32px Arial", fill: "#fff", boundsAlignH: "center", boundsAlignV: "middle" };

function lataus() {
}

function luonti() {
    game.physics.startSystem(Phaser.Physics.ARCADE);
	game.stage.backgroundColor = "#000000";
    salliKuuntelu();
	
	var kuva = luoPallonKuva(32,"#FFFFFF");
	pallo = luoOlio(leveys/2, korkeus/2, kuva);
    pallo.body.bounce.set(1);
    pallo.body.velocity = new Phaser.Point(150, 0);
    pallo.body.onWorldBounds = new Phaser.Signal();
    pallo.body.onWorldBounds.add(tormasiSeinaan);
    pallo.body.setCircle(32);
    
    var mailanKuva = luoKuva(32, 128, "#FFFFFF");
    maila1 = luoOlio(leveys - 32, korkeus/2, mailanKuva);
    maila1.body.immovable = true;
    
    maila2 = luoOlio(0 + 32, korkeus/2, mailanKuva);
    maila2.body.immovable = true;
    
    p1Pistenaytto = luoTeksti(leveys * 0.8, korkeus * 0.15, "0");
    p2Pistenaytto = luoTeksti(leveys * 0.2, korkeus * 0.15, "0");
}


function luoTeksti(x, y, teksti){
    var text = game.add.text(x, y, teksti, tekstinTyyli);
    
    return text;
}


function tormasiSeinaan(tormaaja, ylos, alas, vasen, oikea){
    if(vasen){
        tormaaja.body.position = new Phaser.Point(leveys/2, korkeus/2);
        p1Pisteet++;
        p1Pistenaytto.text = ""+p1Pisteet;
    }
    if(oikea){
        tormaaja.body.position = new Phaser.Point(leveys/2, korkeus/2);
        p2Pisteet++;
        p2Pistenaytto.text = ""+p2Pisteet;
    }
}


function salliKuuntelu(){
    cursors = game.input.keyboard.createCursorKeys();
    wButton = game.input.keyboard.addKey(Phaser.Keyboard.W);
    sButton = game.input.keyboard.addKey(Phaser.Keyboard.S);
}


function luoOlio(x, y, kuva){
	var olio = game.add.sprite(x,y, kuva);
	olio.anchor.x = 0.5;
	olio.anchor.y = 0.5;
    game.physics.enable(olio, Phaser.Physics.ARCADE);
    olio.body.collideWorldBounds = true;
	return olio;
}


function tarkistaLiike(maila, ylos, alas){
    if (ylos.isDown)
    {
        maila.body.velocity.y = -nopeus;
    } else if (alas.isDown){
        maila.body.velocity.y = nopeus;
    }
    else{
        maila.body.velocity.y = 0;
    }
}


function paivitys() {
    game.physics.arcade.collide(pallo, maila1);
    game.physics.arcade.collide(pallo, maila2);
    
    tarkistaLiike(maila1, cursors.up, cursors.down);
    tarkistaLiike(maila2, wButton, sButton);

}