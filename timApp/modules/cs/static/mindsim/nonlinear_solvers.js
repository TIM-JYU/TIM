function test_solver() {
    var ctx = { 'a': 0, 'b': 1, 'c': 0, 'd': -.5 };
    var result = bisection_solver(cubic, ctx, 0, 3., 1e-5, 10);
    result = newton_solver(cubic, dcubic, ctx, result.x, 1e-5, 10);
    alert(result.x + ' ' + result.f);
}

function cubic(x, ctx) {
    return ctx.d + x * (ctx.c + x * ( ctx.b + x * ( ctx.a ) ) );

}
function dcubic(x, ctx) {
    return ctx.c * 1 + x * ( 2 * ctx.b + x * ( ctx.a * 3 ) );
}

function bisection_solver(func, context, xa, xb, ftol, max_iter) {
    console.log('bisection');
    var fa = func(xa, context);
    var fb = func(xb, context);
    for(var i = 0; i < max_iter; ++i) {
	var xc = (xa + xb) / 2.;
	var fc = func(xc, context);
	console.log(xa + ' : ' + fa);
	console.log(xb + ' : ' + fb);
	console.log(xc + ' : ' + fc);
	
	if ( fc*fc < ftol*ftol )
	    return { 'x': xc, 'iters': i };

	if ( fa*fc < 0 ) {
	    xb = xc;
	    fb = fc;
	}
	else if ( fb*fc < 0 ) {
	    xa = xc;
	    fa = fa;
	}
	else {
	    return null;
	}
    }
    return { 'x': .5*(xa+xb), 'iters': i };
}

function newton_solver(func, dfunc, context, x0, ftol, xrtol, max_iter) {
    console.log('newton');
    var xp  = x0;
    var fp  = func(xp, context);
    var dfp = dfunc(xp, context);
    for(var i = 0; i < max_iter; ++i) {
	console.log(xp + ' : ' + fp + ' :: ' + dfp);
	var x  = xp - fp / dfp;
	var f  = func(x, context);
	if ( f*f < ftol*ftol ) {
	    return {'x': x, 'f': f, 'iters': i};
	}
	var tmp = (x-xp)/( Math.min(Math.abs(x),Math.abs(xp) + 1e-15) );
	if ( tmp*tmp < xrtol*xrtol ) {
	    return {'x': x, 'f': f, 'iters': i};
	}
	xp = x;
	fp = f;
	dfp = dfunc(xp, context);
    }
    return {'x': x, 'f': f, 'iters': i};
}
