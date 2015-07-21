using System;
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
            peli.Run();
            peli.RunOneFrame();
       }
    }
}
