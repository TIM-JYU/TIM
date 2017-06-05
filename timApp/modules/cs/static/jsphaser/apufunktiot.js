function luoKuva(leveys, korkeus, vari){
    var bmd = game.add.bitmapData(leveys,korkeus);
    bmd.ctx.beginPath();
    bmd.ctx.rect(0,0,leveys,korkeus);
    bmd.ctx.fillStyle = vari;
    bmd.ctx.fill();
    return bmd;
}

function luoPallonKuva(sade, vari){
	var bmd = game.add.bitmapData(sade*2, sade*2);
    bmd.circle(sade, sade, sade, vari);
	return bmd;
}