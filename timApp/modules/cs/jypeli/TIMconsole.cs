/// <summary>
/// Mock class for echoing ReadLine result in TIM runs
/// </summary>
public class TIMconsole
{
    public static string ReadLine()
    {
        string s = System.Console.ReadLine();
        System.Console.WriteLine(s);
        return s;
    }
}