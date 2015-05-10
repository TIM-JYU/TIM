# Various programming languages

## Java

``` {#helloeka plugin="csPlugin"}
header: Java
type: java/comtest
path: user
byCode: |
 //
 package example;
 public class Laskuja {
     /** 
      * @example 
      * <pre name="test">
      *   summa(2,3) === 5;
      *   summa(-5,5) === 0;
      *  </pre>
      */
     public static int summa(int a, int b) {
         return a + b;
     }
     
     public static void main(String[] args) {
         System.out.println("Summa = " + summa(3,2));
     }
 }
```

``` {#javagra plugin="csPlugin"}
header: Java/Graphics
path: user
type: graphics
byCode: |
    //
    import fi.jyu.mit.graphics.EasyWindow;
    
    public class SimpleGraphics {
    
        public static void main(String[] args) {
            EasyWindow window = new EasyWindow(800,200);
            window.addLine(0,0,100,100);
            window.addCircle(50,50, 20);
        }
    }
```

``` {#javaSwing plugin="csPlugin"}
header: Java/Swing
path: user
delay: 0
prect: 0,0,10000,8000
type: graphics/args
byCode: |
            getTextKartalta().setText(args[0]);
replace: REPLACE
userargs: 20
program: |
 //
    /**
     * 
     */
    package muuttujat.graafinen;
    
    import java.awt.EventQueue;
    import javax.swing.JFrame;
    import javax.swing.JPanel;
    import javax.swing.border.EmptyBorder;
    import java.awt.GridBagLayout;
    import javax.swing.JLabel;
    import java.awt.GridBagConstraints;
    import java.awt.Insets;
    import javax.swing.SwingConstants;
    import javax.swing.JTextField;
    import javax.swing.UIManager;
    import fi.jyu.mit.ohj2.Mjonot;
    import java.awt.event.KeyAdapter;
    import java.awt.event.KeyEvent;
    import java.awt.event.WindowAdapter;
    import java.awt.event.WindowEvent;
    import java.awt.event.ActionListener;
    import java.awt.event.ActionEvent;
    
    /**
     * Ohjelma mittakaavan laskemiseksi
     * @author Vesa Lappalainen @version 1.0, 27.1.2011
     * @author Santtu Viitanen @version 1.1, 03.08.2011
     */
    public class Mittakaava extends JFrame {
    
        /**  */
        private static final long serialVersionUID = 1L;
        private JPanel contentPane;
        private final JLabel lblMittakaava = new JLabel("Mittakaava");
        private final JLabel label = new JLabel("1:");
        private final JTextField textMittakaava = new JTextField();
        private final JLabel lblKartaltaMitattuMatka = new JLabel("Kartalta mitattu matka");
        private final JTextField textKartalta = new JTextField();
        private final JLabel lblMm = new JLabel("mm");
        private final JLabel lblMatkaMaastossa = new JLabel("Matka maastossa");
        private final JTextField textMaastossa = new JTextField();
        private final JLabel lblKm = new JLabel("km");
    
    
        /**
         * @return viite kartalla kenttään
         */
        protected JTextField getTextKartalta() {
            return textKartalta;
        }
    
    
        /**
         * @return viiten mittakaava-kentäään
         */
        protected JTextField getTextMittakaava() {
            return textMittakaava;
        }
    
    
        /**
         * Launch the application.
         * @param args Ei käytössä
         */
        public static void main(String[] args) {
            try {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            } catch (Throwable e) {
                e.printStackTrace();
            }
            EventQueue.invokeLater(new Runnable() {
                @Override
                public void run() {
                    try {
                        Mittakaava frame = new Mittakaava(args);
                        frame.setVisible(true);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            });
        }
    
    
        /**
         * Create the frame.
         */
        public Mittakaava(String[] args) {
            addWindowListener(new WindowAdapter() {
                @Override
                public void windowOpened(WindowEvent arg0) {
                    getTextKartalta().requestFocus();
                }
            });
            textMaastossa.setEditable(false);
            textMaastossa.setColumns(10);
            getTextKartalta().addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    getTextKartalta().selectAll();
                }
            });
            getTextKartalta().addKeyListener(new KeyAdapter() {
                @Override
                public void keyReleased(KeyEvent arg0) {
                    laske();
                }
            });
            getTextKartalta().setColumns(10);
            getTextMittakaava().addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    getTextMittakaava().selectAll();
                }
            });
            getTextMittakaava().addKeyListener(new KeyAdapter() {
                @Override
                public void keyReleased(KeyEvent arg0) {
                    laske();
                }
            });
            getTextMittakaava().setText("200000");
            getTextMittakaava().setColumns(10);
            setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            setBounds(100, 100, 355, 154);
            contentPane = new JPanel();
            contentPane.setBorder(new EmptyBorder(50, 20, 5, 20));
            setContentPane(contentPane);
            GridBagLayout gbl_contentPane = new GridBagLayout();
            gbl_contentPane.columnWidths = new int[]{0, 0, 0, 0, 0, 0};
            gbl_contentPane.rowHeights = new int[]{0, 0, 0, 0, 0};
            gbl_contentPane.columnWeights = new double[]{0.0, 0.0, 0.0, 1.0, 0.0, Double.MIN_VALUE};
            gbl_contentPane.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
            contentPane.setLayout(gbl_contentPane);
            
            GridBagConstraints gbc_lblMittakaava = new GridBagConstraints();
            gbc_lblMittakaava.anchor = GridBagConstraints.WEST;
            gbc_lblMittakaava.insets = new Insets(0, 0, 5, 5);
            gbc_lblMittakaava.gridx = 1;
            gbc_lblMittakaava.gridy = 1;
            contentPane.add(lblMittakaava, gbc_lblMittakaava);
            
            GridBagConstraints gbc_label = new GridBagConstraints();
            gbc_label.insets = new Insets(0, 0, 5, 5);
            gbc_label.anchor = GridBagConstraints.EAST;
            gbc_label.gridx = 2;
            gbc_label.gridy = 1;
            label.setHorizontalAlignment(SwingConstants.RIGHT);
            contentPane.add(label, gbc_label);
            
            GridBagConstraints gbc_textMittakaava = new GridBagConstraints();
            gbc_textMittakaava.insets = new Insets(0, 0, 5, 5);
            gbc_textMittakaava.fill = GridBagConstraints.HORIZONTAL;
            gbc_textMittakaava.gridx = 3;
            gbc_textMittakaava.gridy = 1;
            contentPane.add(getTextMittakaava(), gbc_textMittakaava);
            
            GridBagConstraints gbc_lblKartaltaMitattuMatka = new GridBagConstraints();
            gbc_lblKartaltaMitattuMatka.anchor = GridBagConstraints.WEST;
            gbc_lblKartaltaMitattuMatka.insets = new Insets(0, 0, 5, 5);
            gbc_lblKartaltaMitattuMatka.gridx = 1;
            gbc_lblKartaltaMitattuMatka.gridy = 2;
            contentPane.add(lblKartaltaMitattuMatka, gbc_lblKartaltaMitattuMatka);
            
            GridBagConstraints gbc_textKartalta = new GridBagConstraints();
            gbc_textKartalta.insets = new Insets(0, 0, 5, 5);
            gbc_textKartalta.fill = GridBagConstraints.HORIZONTAL;
            gbc_textKartalta.gridx = 3;
            gbc_textKartalta.gridy = 2;
            contentPane.add(getTextKartalta(), gbc_textKartalta);
            
            GridBagConstraints gbc_lblMm = new GridBagConstraints();
            gbc_lblMm.insets = new Insets(0, 0, 5, 0);
            gbc_lblMm.gridx = 4;
            gbc_lblMm.gridy = 2;
            contentPane.add(lblMm, gbc_lblMm);
            
            GridBagConstraints gbc_lblMatkaMaastossa = new GridBagConstraints();
            gbc_lblMatkaMaastossa.anchor = GridBagConstraints.WEST;
            gbc_lblMatkaMaastossa.insets = new Insets(0, 0, 0, 5);
            gbc_lblMatkaMaastossa.gridx = 1;
            gbc_lblMatkaMaastossa.gridy = 3;
            contentPane.add(lblMatkaMaastossa, gbc_lblMatkaMaastossa);
            
            GridBagConstraints gbc_textMaastossa = new GridBagConstraints();
            gbc_textMaastossa.insets = new Insets(0, 0, 0, 5);
            gbc_textMaastossa.fill = GridBagConstraints.HORIZONTAL;
            gbc_textMaastossa.gridx = 3;
            gbc_textMaastossa.gridy = 3;
            contentPane.add(textMaastossa, gbc_textMaastossa);
            
            GridBagConstraints gbc_lblKm = new GridBagConstraints();
            gbc_lblKm.gridx = 4;
            gbc_lblKm.gridy = 3;
            contentPane.add(lblKm, gbc_lblKm);
            getTextKartalta().setText("20");  // REPLACE
            getTextKartalta().selectAll();
            laske();
            getTextKartalta().requestFocus();
        }
    
        // Oma koodi
        
        /** Oletusmittakaava  */
        public static final double MITTAKAAVA = 200000.0;
        
        /** Kerroin mm muuttamiseksi km */
        public static final double MM_KM = 1000.0 * 1000.0;
        
        
        /**
         * Muunnetaan kartalta mm mitattu matka maastoon km
         * @param matka_mm kartalta mitattu matka mm
         * @param mittakaava käytettävä mittakaava
         * @return matka maastossa km
         */
        public static double mittakaavamuunnos(double matka_mm, double mittakaava) {
            return mittakaava*matka_mm / MM_KM;
        }
        
        
        /**
         * Haetaan tekstikentästä luku
         * @param text kenttä josta haetaan
         * @param oletus luvun arvo jos kentästä ei saa järkevää lukua
         * @return kentästä haettu luku
         * @example
         * <pre name="test">
         * #TOLERANCE=0.01
         * #import javax.swing.JTextField;
         * haeLuku(new JTextField("234.233"), 44) ~~~ 234.23;
         * haeLuku(new JTextField("foobar1"), 44) ~~~ 44.00;
         * haeLuku(new JTextField("1foobar"), 44) ~~~ 1.00;
         * </pre>
         */
        public static double haeLuku(JTextField text,double oletus) {
            double luku = Mjonot.erotaDouble(text.getText(), oletus);
            return luku;
        }
        
        
        /**
         * Laittaa tekstikenttään tuloksen
         * @param text kenttä johon luku laitetaan
         * @param luku joka laitetaan kenttään
         */
        public static void laitaTulos(JTextField text, double luku) {
            String tulos = String.format("%5.2f",luku);
            tulos = tulos.replace(',', '.');
            text.setText(tulos);
        }
        
        
        /**
         * Lasketaan uudet arvot muuttuneiden perusteella
         */
        protected void laske() {
            double mittakaava = haeLuku(getTextMittakaava(),MITTAKAAVA); 
            double matka_mm = haeLuku(getTextKartalta(), 0);
            double matka_km = mittakaavamuunnos(matka_mm,mittakaava);
            laitaTulos(textMaastossa,matka_km);
        }
        
        
        
    }
```

``` {#javagra3D plugin="csPlugin"}
header: Java/Graphics 3D pinta
path: user
type: graphics
program: |
 package graphics;
 
 import fi.jyu.mit.graphics.*;
 import static java.lang.Math.*; 
 
 
 /**
  * Esimerkki 3D-pinnan piirtämisestä.
  * Piirtää monta rinnakkaista sin-käyrää ja pyörittää kuvaa.
  * @author vesal
  */
 public class FunctionR2Sample {
     /** @param args ei käytössä */
     public static void main(String[] args) {
         EasyWindow window = new EasyWindow(600,600);
         window.scale(-9,-9,9,9);
         window.addAxis(15, 15, 15);
         // FUNCTION
     }
 }

replace: FUNCTION
byCode: |
 //
         window.add((x,y)->sin(x)*cos(y),-2*PI,-2*PI,2*PI,2*PI);  
         window.rotate(Axis.X, 50);
         window.rotate(Axis.Y, 20);
```

## C\#

``` {#hellocs plugin="csPlugin"}
header: C#
type: cs/comtest
byCode: |
 //C#
 public class Laskuja
 {
     /// <example>
     /// <pre name="test">
     ///    Laskuja.Summa(2,3) === 5;
     ///    Laskuja.Summa(-5,5) === 0;
     /// </pre>
     /// </example>     
     public static int Summa(int a, int b)
     {
         return a + b;
     }
 
     public static void Main()
     {
         System.Console.WriteLine("Summa = " + Summa(2,3));
     }
 }
```

``` {#punainenympyra plugin="csPlugin"}
header: C# / JyPeli
type: jypeli 
maxrows: 20
file: https://svn.cc.jyu.fi/srv/svn/ohj1/luentomonistecs/esimerkit/Pohja/Jypeli/Jypeli.cs
replace: INSERT YOUR CODE HERE
byCode: |
 //
         Level.Background.Color = Color.Black;
         PhysicsObject pallo = new PhysicsObject(200,200,Shape.Circle);
         pallo.Color = Color.Yellow;
         Add(pallo);
```

## C

``` {#helloc plugin="csPlugin"}
header: C
type: cc
byCode: |
    #include <stdio.h>
    int summa(int a, int b) {
      return a + b;
    } 
    
    int main(void)
    {
      printf("Summa = %d\n",summa(2,3));
      return 0;
    }
```

``` {#helloccc plugin="csPlugin"}
header: C ohjelma käännettynä C++:lla
type: c++
byCode: |
 //
 // C++ kääntäjällä
 #include <stdio.h>
 int main(void)
 {
   printf("Terve! Olen C-kielellä kirjoitettu ohjelma.\n");
   return 0;
 }
```

## C++

``` {#hellocpp plugin="csPlugin"}
header: C++
type: c++/comtest
byCode: |
 //
 // C++ -kieli
 #include <iostream>
 using namespace std;
 
 /** 
  * @example 
  * <pre name="test">
  *   summa(2,3) === 5;
  *   summa(-5,5) === 0;
  *  </pre>
  */
 int summa(int a, int b) {
     return a + b;
 }
     
 int main()
 {
     cout << "Summa = " << summa(2,3) << endl;
     return 0;
 }
```

## Python

``` {#hellopy plugin="csPlugin"}
header: Python 3
type: py
byCode: |
 # Python
 def summa(a,b):
    return a+b
    
 print("Summa = %d" % summa(2,3))    
```

## F\#

``` {#hellofs plugin="csPlugin"}
header: F#
type: fs
byCode: |
 printfn "Hello World"
```

## Lisp

``` {#helloclisp plugin="csPlugin"}
header: Common Lisp (sbcl)
type: clisp
byCode: |
 ;; Common Lisp
 (defun summa (a b)
   "Laskee annetun kahden luvun summan."  ;; Näin tehdään docstring.
   (+ a b))

 (fresh-line)
 (format t "Summa = ~d~%" (summa 2 3))
```

## JavaScript

``` {#jsLumiukko2 plugin="csPlugin"}
type: js
stem: Muokkaa alla olevan lumiukko-ohjelman numeerisia arvoja ja tutki mikä vaikuttaa mihinkäkin.
button: Tallenna
height: 300
header: JavaScript
iframe: True
autorun: true
autoupdate: 200
program: |!!
function paint(ctx,out) {
  "use strict";  // Varoittaa mm. esittelemättömien muuttujien käytöstä

  function clear(c) {
    c.save();          // Store the current transformation matrix
    c.setTransform(1, 0, 0, 1, 0, 0); // Use the identity matrix
                                      // while clearing the canvas
    c.clearRect(0, 0, c.canvas.width, c.canvas.height);
    c.restore();       // Restore the transform
  }

  function circle(ctx, x,y,r) {
     ctx.beginPath();
     ctx.arc(x, y, r, 0, Math.PI * 2, false);
     ctx.stroke();
     ctx.fill();
     ctx.closePath();
  } 

  ctx.fillStyle = "lightGray";
  ctx.strokeStyle = "black";
  ctx.lineWidth = 2;

  clear(ctx);

  ctx.scale(1, 1);
  // REPLACE
}
!!
replace: REPLACE
byCode: |!!
//
  var x1 = 100;
  var y1 = 200;
  circle(ctx, x1, y1, 50);

  var x2 = x1; 
  var y2 = y1 - 50 - 30;
  circle(ctx, x2, y2, 30);

  var x3 = x2 + 10;  // Vaihda tähän arvoa
  var y3 = y2 - 30 - 20;
  ctx.fillStyle = "yellow";
  circle(ctx, x3, y3, 20);
  out.writeln("Hello world!");
!!
```

``` {#loopJS plugin="csPlugin"}
header: JavaScript palvelimella
type: jjs
byCode: |
    function sum(a,b) {
       return a + b;
    }
    
    print("Summa = " + sum(2,3));
```

``` {#jsbin plugin="showVideo"}
header: JavaScript bin
iframe: true
nopen: true
videoicon: false
hidetext: Piilota JSBin
type: small
videoname: "Avaa tästä JSBin"
width: 800
height: 550
file: "https://jsbin.com/iwovaj/74/embed?js,output"
```

## Float ongelma

``` {#floatvika plugin="csPlugin"}
header: Float ongelma / C#
program: |
 //
 using System;
 public class FloatOngelma
 {
     public static void Main()
     {
         // CODE
     }
 }
replace: CODE
byCode: |
 //
         float s = 0;
         float d = 0.1f;
         for (int i=0; i<1000; i++) s += d;
         Console.WriteLine("{0:0.00000000}",s);
```

## SQL

``` {#sqlMalli plugin="csPlugin"}
header: SQL
type: sql
dbname: kerho
path: user
rows: 1
byCode: |
    DROP TABLE Jasenet;
    CREATE TABLE Jasenet (
    JID INTEGER PRIMARY KEY AUTOINCREMENT,
    Nimi VARCHAR(100) NOT NULL,
    Hetu VARCHAR(11) DEFAULT '',
    Osoite  VARCHAR(100) NOT NULL
    );
    
    INSERT INTO Jasenet (Nimi, Hetu, Osoite) VALUES ('Ankka Aku','010245-123U','Ankkakuja');
    INSERT INTO Jasenet (Nimi, Hetu, Osoite) VALUES ('Susi Sepe','020347-123T','Susihaara 7');
    INSERT INTO Jasenet (Nimi, Hetu, Osoite) VALUES ('Ponteva Veli','030455-3333','Possuntaival 2');
    
    
    SELECT * FROM Jasenet;
```

``` {#sqlKysely plugin="csPlugin"}
type: sql
dbname: kerho
path: user
rows: 1
byCode: |
    SELECT * FROM Jasenet WHERE Nimi LIKE '%a%';
```

# Input ja args eri kielillä

Seuraavassa esimerkkejä miten standardi inputtia ja komentorivin
agrumenttejä käsitellään eri kielillä.


## Java

``` {#inputJava plugin="csPlugin"}
type: java/input/args
header: java
inputplaceholder: "Kirjoita ohjelman syöte tähän, kukin jono omalle rivilleen"
inputstem: "Anna syöte"
inputrows: 3
stdin: input.txt
byCode: |
 package kokeiluja;
 
 import java.util.Scanner;

 /**
  * @author vesal
  * @version 16.1.2015
  */
 public class LueJonoja {
 
     /**
      * @param args ei käytössä
      */
     public static void main(String[] args)  {
        for (int i=0; i<args.length; i++) 
            System.out.printf("%2s: %s\n",i, args[i]);
        
     
         Scanner sc = new Scanner(System.in);
         while ( sc.hasNextLine() ) {
             String input = sc.nextLine();
             if ( input == null ) break;
             System.out.println("Syöttö oli: " + input);
         }
         System.out.println("Loppu");
     }
 }
```

## C\#

``` {#inputCS plugin="csPlugin"}
type: cs/input/args
header: C#
inputplaceholder: "Kirjoita ohjelman syöte tähän, kukin jono omalle rivilleen"
inputstem: "Anna syöte"
inputrows: 3
stdin: input.txt
byCode: |
 using System;
 
 /// <summary>
 /// Harjoitellaan do-while-silmukan käyttöä.
 /// </summary>
 public class NimenTulostus
 {
     /// <summary>
     /// Pyydetään käyttäjältä syöte ja tulostellaan.
     /// </summary>
     public static void Main(string[] args)
     {
         for (int i=0; i<args.Length; i++)
             Console.WriteLine("{0,2}: {1}",i,args[i]); 
     
         String nimi;
         do
         {
             Console.Write("Anna nimi > ");
             nimi = Console.ReadLine();
         } while (nimi != null && nimi.Length == 0);
         Console.WriteLine();
         Console.WriteLine("Hei, " + nimi + "!");
     }
 }
```

## Python

``` {#inputPY3 plugin="csPlugin"}
type: py3/input/args
header: Python 3
inputplaceholder: "Kirjoita ohjelman syöte tähän, kukin jono omalle rivilleen"
inputstem: "Anna syöte"
inputrows: 3
stdin: input.txt
byCode: |
 import sys
 print('Argument List:', str(sys.argv))
 input_var = input("Enter something: ")
 print ("you entered " + input_var) 
```

## C++

``` {#inputCPP plugin="csPlugin"}
type: c++/input/args
header: C++
inputplaceholder: "Kirjoita ohjelman syöte tähän, kukin jono omalle rivilleen"
inputstem: "Anna syöte"
inputrows: 3
stdin: input.txt
byCode: |
 //
 // C++ -kieli
 #include <iostream>
 #include <iomanip>
 using namespace std;
     
 int main(int argc, char **argv)
 {
     for (int i=0; i<argc; i++)
        cout << setw(2) << i << ": " << argv[i] << endl;
     string s;
     cout << "Anna jotakin >";
     getline(cin,s);
     cout << "Annoit: " << s;
     return 0;
 }  
```

## C

``` {#inputC plugin="csPlugin"}
type: cc/input/args
header: C
inputplaceholder: "Kirjoita ohjelman syöte tähän, kukin jono omalle rivilleen"
inputstem: "Anna syöte"
inputrows: 3
stdin: input.txt
byCode: |
 #include <stdio.h>
 int main(int argc, char **argv)
 {
    int i;
    for (i=0; i<argc; i++)   
        printf("%2d: %s\n",i,argv[i]);
    
    char s[50];
    printf("Anna jotakin >");
    fgets(s, sizeof(s), stdin);
    printf("\nAnnoit: %s\n",s);
    return 0;
 }
```

## Lisp

``` {#inputLisp plugin="csPlugin"}
header: Common Lisp (sbcl)
type: clisp/input/args
inputplaceholder: "Kirjoita ohjelman syöte tähän, kukin jono omalle rivilleen"
inputstem: "Anna syöte"
inputrows: 3
stdin: input.txt
byCode: |
 (format t "~&~S~&" *posix-argv*)
 (format t "Enter some text: ")
 (let ((s (read-line)))
     (format t "You entered ~s~%" s))
  
 (format t "Enter a number: ")
 (let ((n (read)))
     (if (numberp n)
         (format t "You entered ~d.~%" n)
       (format t "That was not a number.")))
```

## F\#

``` {#inputFS plugin="csPlugin"}
header: F#
type: fs/input/args
inputplaceholder: "Kirjoita ohjelman syöte tähän, kukin jono omalle rivilleen"
inputstem: "Anna syöte"
inputrows: 3
stdin: input.txt
byCode: |
 open System

 let main(args) =
     printfn "args: %A" <| Environment.GetCommandLineArgs() 
     Console.Write("Enter your input: ")
     let input = Console.ReadLine()
     Console.Write("You entered: {0}", input)

 main()
```

## JavaScript

``` {#inputJS plugin="csPlugin"}
header: JavaScript palvelimella
type: jjs/input/args
path: user
inputplaceholder: "Kirjoita ohjelman syöte tähän, kukin jono omalle rivilleen"
inputstem: "Anna syöte"
inputrows: 3
stdin: input.txt
userargs: "-- eka toka kolmas"
rows: 5
byCode: |
  print("Hello World");
  print(arguments);
```

## Shell

``` {#inputShell plugin="csPlugin"}
header: Shell
type: shell/input/args
path: user
inputplaceholder: "Kirjoita ohjelman syöte tähän, kukin jono omalle rivilleen"
inputstem: "Anna syöte"
inputrows: 3
stdin: input.txt
rows: 10
byCode: |
 #!/bin/sh
 ls 
 cat input.txt
 echo $1
```

Konsoli toisenlaisella käyttöliittymällä


``` {#consoletest plugin="csPlugin"}
type: csconsole/shell
path: user
byCode: |           
 //
        System.out.println("Hello World!");
examples:
    - title: "Listataan hakemistot"
      expr: "ls -la"
    - title: "Poistetaan tiedosto"
      expr: "rm foo"
    - title: "prosessit"
      expr: "ps -ef"
```