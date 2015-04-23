using System;
using System.Collections.Generic;
using System.IO;
using Jypeli;

/// <summary>
/// The main class.
/// </summary>
public static class Program
{
    /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    static void Main(string[] args)
    {
       using (var peli = new Peli())
       {
            peli.RunOneFrame();

            string imgfile = args.Length > 0 ? args[0] : "screen.bmp";
            FileStream screenFile = new FileStream( imgfile, FileMode.Create );
            Screencap.WriteBmp( screenFile, Peli.Screen.Image );
            screenFile.Close();
       }
    }
}
