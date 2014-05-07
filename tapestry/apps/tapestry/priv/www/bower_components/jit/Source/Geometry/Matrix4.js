/*
 * Matrix4 class based on three.js http://github.com/mrdoob/three.js, Copyright (c) Mr.doob http://mrdoob.com/, MIT License http://github.com/mrdoob/three.js/blob/master/LICENSE 
 */

var Matrix4 = function() {
	this._x = new Vector3();
	this._y = new Vector3();
	this._z = new Vector3();
};

$jit.Matrix4 = Matrix4;

Matrix4.prototype = {

	n11: 1, n12: 0, n13: 0, n14: 0,
	n21: 0, n22: 1, n23: 0, n24: 0,
	n31: 0, n32: 0, n33: 1, n34: 0,
	n41: 0, n42: 0, n43: 0, n44: 1,

	identity: function() {
		this.n11 = 1; this.n12 = 0; this.n13 = 0; this.n14 = 0;
		this.n21 = 0; this.n22 = 1; this.n23 = 0; this.n24 = 0;
		this.n31 = 0; this.n32 = 0; this.n33 = 1; this.n34 = 0;
		this.n41 = 0; this.n42 = 0; this.n43 = 0; this.n44 = 1;
	},

	lookAt: function(eye, center, up) {
		var x = this._x, y = this._y, z = this._z;

		z.sub(eye, center);
		z.normalize();

		x.cross(up, z);
		x.normalize();

		y.cross(z, x);
		y.normalize();

		this.n11 = x.x; this.n12 = x.y; this.n13 = x.z; this.n14 = - x.dot( eye );
		this.n21 = y.x; this.n22 = y.y; this.n23 = y.z; this.n24 = - y.dot( eye );
		this.n31 = z.x; this.n32 = z.y; this.n33 = z.z; this.n34 = - z.dot( eye );
	},

	transform: function(v) {
		var vx = v.x, vy = v.y, vz = v.z, vw = v.w ? v.w : 1.0;

		v.x = this.n11 * vx + this.n12 * vy + this.n13 * vz + this.n14 * vw;
		v.y = this.n21 * vx + this.n22 * vy + this.n23 * vz + this.n24 * vw;
		v.z = this.n31 * vx + this.n32 * vy + this.n33 * vz + this.n34 * vw;

		vw = this.n41 * vx + this.n42 * vy + this.n43 * vz + this.n44 * vw;

		if(v.w) {
			v.w = vw;
		} else {
			v.x = v.x / vw;
			v.y = v.y / vw;
			v.z = v.z / vw;
		}
	},

	multiply: function(a, b) {
		this.n11 = a.n11 * b.n11 + a.n12 * b.n21 + a.n13 * b.n31 + a.n14 * b.n41;
		this.n12 = a.n11 * b.n12 + a.n12 * b.n22 + a.n13 * b.n32 + a.n14 * b.n42;
		this.n13 = a.n11 * b.n13 + a.n12 * b.n23 + a.n13 * b.n33 + a.n14 * b.n43;
		this.n14 = a.n11 * b.n14 + a.n12 * b.n24 + a.n13 * b.n34 + a.n14 * b.n44;

		this.n21 = a.n21 * b.n11 + a.n22 * b.n21 + a.n23 * b.n31 + a.n24 * b.n41;
		this.n22 = a.n21 * b.n12 + a.n22 * b.n22 + a.n23 * b.n32 + a.n24 * b.n42;
		this.n23 = a.n21 * b.n13 + a.n22 * b.n23 + a.n23 * b.n33 + a.n24 * b.n43;
		this.n24 = a.n21 * b.n14 + a.n22 * b.n24 + a.n23 * b.n34 + a.n24 * b.n44;

		this.n31 = a.n31 * b.n11 + a.n32 * b.n21 + a.n33 * b.n31 + a.n34 * b.n41;
		this.n32 = a.n31 * b.n12 + a.n32 * b.n22 + a.n33 * b.n32 + a.n34 * b.n42;
		this.n33 = a.n31 * b.n13 + a.n32 * b.n23 + a.n33 * b.n33 + a.n34 * b.n43;
		this.n34 = a.n31 * b.n14 + a.n32 * b.n24 + a.n33 * b.n34 + a.n34 * b.n44;

		this.n41 = a.n41 * b.n11 + a.n42 * b.n21 + a.n43 * b.n31 + a.n44 * b.n41;
		this.n42 = a.n41 * b.n12 + a.n42 * b.n22 + a.n43 * b.n32 + a.n44 * b.n42;
		this.n43 = a.n41 * b.n13 + a.n42 * b.n23 + a.n43 * b.n33 + a.n44 * b.n43;
		this.n44 = a.n41 * b.n14 + a.n42 * b.n24 + a.n43 * b.n34 + a.n44 * b.n44;
	},

	$multiply: function(m) {
		var n11 = this.n11, n12 = this.n12, n13 = this.n13, n14 = this.n14,
    		n21 = this.n21, n22 = this.n22, n23 = this.n23, n24 = this.n24,
    		n31 = this.n31, n32 = this.n32, n33 = this.n33, n34 = this.n34,
    		n41 = this.n41, n42 = this.n42, n43 = this.n43, n44 = this.n44;

		this.n11 = n11 * m.n11 + n12 * m.n21 + n13 * m.n31 + n14 * m.n41;
		this.n12 = n11 * m.n12 + n12 * m.n22 + n13 * m.n32 + n14 * m.n42;
		this.n13 = n11 * m.n13 + n12 * m.n23 + n13 * m.n33 + n14 * m.n43;
		this.n14 = n11 * m.n14 + n12 * m.n24 + n13 * m.n34 + n14 * m.n44;

		this.n21 = n21 * m.n11 + n22 * m.n21 + n23 * m.n31 + n24 * m.n41;
		this.n22 = n21 * m.n12 + n22 * m.n22 + n23 * m.n32 + n24 * m.n42;
		this.n23 = n21 * m.n13 + n22 * m.n23 + n23 * m.n33 + n24 * m.n43;
		this.n24 = n21 * m.n14 + n22 * m.n24 + n23 * m.n34 + n24 * m.n44;

		this.n31 = n31 * m.n11 + n32 * m.n21 + n33 * m.n31 + n34 * m.n41;
		this.n32 = n31 * m.n12 + n32 * m.n22 + n33 * m.n32 + n34 * m.n42;
		this.n33 = n31 * m.n13 + n32 * m.n23 + n33 * m.n33 + n34 * m.n43;
		this.n34 = n31 * m.n14 + n32 * m.n24 + n33 * m.n34 + n34 * m.n44;

		this.n41 = n41 * m.n11 + n42 * m.n21 + n43 * m.n31 + n44 * m.n41;
		this.n42 = n41 * m.n12 + n42 * m.n22 + n43 * m.n32 + n44 * m.n42;
		this.n43 = n41 * m.n13 + n42 * m.n23 + n43 * m.n33 + n44 * m.n43;
		this.n44 = n41 * m.n14 + n42 * m.n24 + n43 * m.n34 + n44 * m.n44;
	},

	$scale: function(s) {
		this.n11 *= s; this.n12 *= s; this.n13 *= s; this.n14 *= s;
		this.n21 *= s; this.n22 *= s; this.n23 *= s; this.n24 *= s;
		this.n31 *= s; this.n32 *= s; this.n33 *= s; this.n34 *= s;
		this.n41 *= s; this.n42 *= s; this.n43 *= s; this.n44 *= s;
		return this;
	},
	
	$add: function(m) {
	  this.n11 += m.n11;
	  this.n12 += m.n12;
	  this.n13 += m.n13;
	  this.n14 += m.n14;
	  this.n21 += m.n21;
	  this.n22 += m.n22;
	  this.n23 += m.n23;
	  this.n24 += m.n24;
	  this.n31 += m.n31;
	  this.n32 += m.n32;
	  this.n33 += m.n33;
	  this.n34 += m.n34;
	  this.n41 += m.n41;
	  this.n42 += m.n42;
	  this.n43 += m.n43;
	  this.n44 += m.n44;
	  return this;
	},

	determinant: function() {
		return (
			this.n14 * this.n23 * this.n32 * this.n41-
			this.n13 * this.n24 * this.n32 * this.n41-
			this.n14 * this.n22 * this.n33 * this.n41+
			this.n12 * this.n24 * this.n33 * this.n41+

			this.n13 * this.n22 * this.n34 * this.n41-
			this.n12 * this.n23 * this.n34 * this.n41-
			this.n14 * this.n23 * this.n31 * this.n42+
			this.n13 * this.n24 * this.n31 * this.n42+

			this.n14 * this.n21 * this.n33 * this.n42-
			this.n11 * this.n24 * this.n33 * this.n42-
			this.n13 * this.n21 * this.n34 * this.n42+
			this.n11 * this.n23 * this.n34 * this.n42+

			this.n14 * this.n22 * this.n31 * this.n43-
			this.n12 * this.n24 * this.n31 * this.n43-
			this.n14 * this.n21 * this.n32 * this.n43+
			this.n11 * this.n24 * this.n32 * this.n43+

			this.n12 * this.n21 * this.n34 * this.n43-
			this.n11 * this.n22 * this.n34 * this.n43-
			this.n13 * this.n22 * this.n31 * this.n44+
			this.n12 * this.n23 * this.n31 * this.n44+

			this.n13 * this.n21 * this.n32 * this.n44-
			this.n11 * this.n23 * this.n32 * this.n44-
			this.n12 * this.n21 * this.n33 * this.n44+
			this.n11 * this.n22 * this.n33 * this.n44 );
	},

  $transpose: function() {
	  function swap(obj, p1, p2) {
	    var aux = obj[p1];
	    obj[p1] = obj[p2];
	    obj[p2] = aux;
	  }
	  
	  swap(this, 'n21', 'n12');
	  swap(this, 'n31', 'n13');
	  swap(this, 'n32', 'n23');
	  swap(this, 'n41', 'n14');
	  swap(this, 'n42', 'n24');
	  swap(this, 'n43', 'n34');
	  return this;
  },
  
	clone: function() {
		var m = new Matrix4();
		m.n11 = this.n11; m.n12 = this.n12; m.n13 = this.n13; m.n14 = this.n14;
		m.n21 = this.n21; m.n22 = this.n22; m.n23 = this.n23; m.n24 = this.n24;
		m.n31 = this.n31; m.n32 = this.n32; m.n33 = this.n33; m.n34 = this.n34;
		m.n41 = this.n41; m.n42 = this.n42; m.n43 = this.n43; m.n44 = this.n44;
		return m;
	},
	
	flatten: function() {
	  return [this.n11, this.n21, this.n31, this.n41,
	      this.n12, this.n22, this.n32, this.n42,
	      this.n13, this.n23, this.n33, this.n43,
	      this.n14, this.n24, this.n34, this.n44];
	}
};

Matrix4.translationMatrix = function(x, y, z) {
	var m = new Matrix4();

	m.n14 = x;
	m.n24 = y;
	m.n34 = z;

	return m;
};

Matrix4.scaleMatrix = function(x, y, z) {
	var m = new Matrix4();

	m.n11 = x;
	m.n22 = y;
	m.n33 = z;

	return m;
};

Matrix4.rotationXMatrix = function(theta) {
	var rot = new Matrix4();

	rot.n22 = rot.n33 = Math.cos( theta );
	rot.n32 = Math.sin( theta );
	rot.n23 = - rot.n32;

	return rot;
};

Matrix4.rotationYMatrix = function(theta) {
	var rot = new Matrix4();

	rot.n11 = rot.n33 = Math.cos( theta );
	rot.n13 = Math.sin( theta );
	rot.n31 = - rot.n13;

	return rot;
};

Matrix4.rotationZMatrix = function(theta) {
	var rot = new Matrix4();

	rot.n11 = rot.n22 = Math.cos( theta );
	rot.n21 = Math.sin( theta );
	rot.n12 = - rot.n21;

	return rot;
};

Matrix4.makeInvert = function(m1) {
	var m2 = new Matrix4();

	m2.n11 = m1.n23*m1.n34*m1.n42 - m1.n24*m1.n33*m1.n42 + m1.n24*m1.n32*m1.n43 - m1.n22*m1.n34*m1.n43 - m1.n23*m1.n32*m1.n44 + m1.n22*m1.n33*m1.n44;
	m2.n12 = m1.n14*m1.n33*m1.n42 - m1.n13*m1.n34*m1.n42 - m1.n14*m1.n32*m1.n43 + m1.n12*m1.n34*m1.n43 + m1.n13*m1.n32*m1.n44 - m1.n12*m1.n33*m1.n44;
	m2.n13 = m1.n13*m1.n24*m1.n42 - m1.n14*m1.n23*m1.n42 + m1.n14*m1.n22*m1.n43 - m1.n12*m1.n24*m1.n43 - m1.n13*m1.n22*m1.n44 + m1.n12*m1.n23*m1.n44;
	m2.n14 = m1.n14*m1.n23*m1.n32 - m1.n13*m1.n24*m1.n32 - m1.n14*m1.n22*m1.n33 + m1.n12*m1.n24*m1.n33 + m1.n13*m1.n22*m1.n34 - m1.n12*m1.n23*m1.n34;
	m2.n21 = m1.n24*m1.n33*m1.n41 - m1.n23*m1.n34*m1.n41 - m1.n24*m1.n31*m1.n43 + m1.n21*m1.n34*m1.n43 + m1.n23*m1.n31*m1.n44 - m1.n21*m1.n33*m1.n44;
	m2.n22 = m1.n13*m1.n34*m1.n41 - m1.n14*m1.n33*m1.n41 + m1.n14*m1.n31*m1.n43 - m1.n11*m1.n34*m1.n43 - m1.n13*m1.n31*m1.n44 + m1.n11*m1.n33*m1.n44;
	m2.n23 = m1.n14*m1.n23*m1.n41 - m1.n13*m1.n24*m1.n41 - m1.n14*m1.n21*m1.n43 + m1.n11*m1.n24*m1.n43 + m1.n13*m1.n21*m1.n44 - m1.n11*m1.n23*m1.n44;
	m2.n24 = m1.n13*m1.n24*m1.n31 - m1.n14*m1.n23*m1.n31 + m1.n14*m1.n21*m1.n33 - m1.n11*m1.n24*m1.n33 - m1.n13*m1.n21*m1.n34 + m1.n11*m1.n23*m1.n34;
	m2.n31 = m1.n22*m1.n34*m1.n41 - m1.n24*m1.n32*m1.n41 + m1.n24*m1.n31*m1.n42 - m1.n21*m1.n34*m1.n42 - m1.n22*m1.n31*m1.n44 + m1.n21*m1.n32*m1.n44;
	m2.n32 = m1.n14*m1.n32*m1.n41 - m1.n12*m1.n34*m1.n41 - m1.n14*m1.n31*m1.n42 + m1.n11*m1.n34*m1.n42 + m1.n12*m1.n31*m1.n44 - m1.n11*m1.n32*m1.n44;
	m2.n33 = m1.n13*m1.n24*m1.n41 - m1.n14*m1.n22*m1.n41 + m1.n14*m1.n21*m1.n42 - m1.n11*m1.n24*m1.n42 - m1.n12*m1.n21*m1.n44 + m1.n11*m1.n22*m1.n44;
	m2.n34 = m1.n14*m1.n22*m1.n31 - m1.n12*m1.n24*m1.n31 - m1.n14*m1.n21*m1.n32 + m1.n11*m1.n24*m1.n32 + m1.n12*m1.n21*m1.n34 - m1.n11*m1.n22*m1.n34;
	m2.n41 = m1.n23*m1.n32*m1.n41 - m1.n22*m1.n33*m1.n41 - m1.n23*m1.n31*m1.n42 + m1.n21*m1.n33*m1.n42 + m1.n22*m1.n31*m1.n43 - m1.n21*m1.n32*m1.n43;
	m2.n42 = m1.n12*m1.n33*m1.n41 - m1.n13*m1.n32*m1.n41 + m1.n13*m1.n31*m1.n42 - m1.n11*m1.n33*m1.n42 - m1.n12*m1.n31*m1.n43 + m1.n11*m1.n32*m1.n43;
	m2.n43 = m1.n13*m1.n22*m1.n41 - m1.n12*m1.n23*m1.n41 - m1.n13*m1.n21*m1.n42 + m1.n11*m1.n23*m1.n42 + m1.n12*m1.n21*m1.n43 - m1.n11*m1.n22*m1.n43;
	m2.n44 = m1.n12*m1.n23*m1.n31 - m1.n13*m1.n22*m1.n31 + m1.n13*m1.n21*m1.n32 - m1.n11*m1.n23*m1.n32 - m1.n12*m1.n21*m1.n33 + m1.n11*m1.n22*m1.n33;
	m2.$scale(1 / m1.determinant());

	return m2;
};

Matrix4.makeFrustum = function( left, right, bottom, top, near, far ) {
	var m, x, y, a, b, c, d;

	m = new Matrix4();
	x = 2 * near / ( right - left );
	y = 2 * near / ( top - bottom );
	a = ( right + left ) / ( right - left );
	b = ( top + bottom ) / ( top - bottom );
	c = - ( far + near ) / ( far - near );
	d = - 2 * far * near / ( far - near );

	m.n11 = x;  m.n12 = 0;  m.n13 = a;   m.n14 = 0;
	m.n21 = 0;  m.n22 = y;  m.n23 = b;   m.n24 = 0;
	m.n31 = 0;  m.n32 = 0;  m.n33 = c;   m.n34 = d;
	m.n41 = 0;  m.n42 = 0;  m.n43 = - 1; m.n44 = 0;

	return m;
};

Matrix4.makePerspective = function( fov, aspect, near, far ) {
	var ymax, ymin, xmin, xmax;

	ymax = near * Math.tan( fov * Math.PI / 360 );
	ymin = - ymax;
	xmin = ymin * aspect;
	xmax = ymax * aspect;

	return Matrix4.makeFrustum( xmin, xmax, ymin, ymax, near, far );
};

Matrix4.makeOrtho = function( left, right, top, bottom, near, far ) {
	var m, x, y, z, w, h, p;

	m = new Matrix4();
	w = right - left;
	h = bottom - top;
	p = far - near;
	x = ( right + left ) / w;
	y = ( bottom + top ) / h;
	z = ( far + near ) / p;

	m.n11 = 2 / w; m.n12 = 0;     m.n13 = 0;      m.n14 = -x;
	m.n21 = 0;     m.n22 = 2 / h; m.n23 = 0;      m.n24 = -y;
	m.n31 = 0;     m.n32 = 0;     m.n33 = -2 / p; m.n34 = -z;
	m.n41 = 0;     m.n42 = 0;     m.n43 = 0;      m.n44 = 1;

	return m;
};
