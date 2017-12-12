using System;
using System.Text;
using System.Linq;
using System.Collections.Generic;
using NUnit.Framework;
using static Peli;

	[TestFixture()]
	public  class TestPeli
	{
		[Test()]
		public  void TestPienintenLukumaara39()
		{
			Assert.AreEqual( 4, Peli.PienintenLukumaara(new int[]{2,3,2,3,2,2, 2}, 1, 0) , "in method PienintenLukumaara, line 40");
		}
	}
