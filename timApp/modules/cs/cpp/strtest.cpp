#include <iostream.h>
#include <string>
using namespace std;

int main(void)
{
  string mjono1 = "Ensimmainen";
  string mjono2 = "Toinen";
  string mjono;

// syotto ja tulostus
  cout << "Anna merkkijono    > ";
  getline(cin,mjono,'\n');
  cout << "Annoit merkkijonon : " << mjono << endl;

// kasittely merkeittain
  mjono[0] = mjono1[4];
  mjono[1] = mjono2[1];
  mjono[2] = 't';
  mjono[3] = '\0';       // Laitonta jos merkkijono ei ole nain iso!
                         // Ei vaikuta!
  cout << mjono << endl; // tulostaa: mot ....

// sijoitukset
  mjono = mjono1;
  cout << mjono << endl;       // Ensimmainen

  mjono = "Eka";
  cout << mjono << endl;       // Eka

// katenointi
  mjono = mjono1 + mjono2;
  cout << mjono << endl;       // EnsimmainenToinen

  mjono = mjono1 + "Toka";
  cout << mjono << endl;       // EnsimmainenToka

  mjono = "Eka" + mjono2;
  cout << mjono << endl;       // EkaToinen

// vertailut
  if (mjono1 == mjono2) cout << "1 on 2" << endl;           // ei tulosta
  if (mjono1 == "Apua") cout << "1 on Apua" << endl;        // ei tulosta
  if ("Apua" == mjono2) cout << "Apua on 2" << endl;        // ei tulosta

  if (mjono1 != mjono2) cout << "1 ei ole 2" << endl;       // 1 ei ole 2
  if (mjono1 != "Apua") cout << "1 ei ole Apua" << endl;    // 1 ei ole Apua
  if ("Apua" != mjono2) cout << "Apua ei ole 2" << endl;    // Apua ei ole 2

  if (mjono1 < mjono2) cout << "1 pienempi kuin 2" << endl;     // 1 pienempi ku
  if (mjono1 < "Apua") cout << "1 pienempi kuin Apua" << endl;  // ei tulosta
  if ("Apua" < mjono2) cout << "Apua pienempi kuin 2" << endl;  // Apua pienempi

  if (mjono1 > mjono2) cout << "1 suurempi kuin 2" << endl;     // ei tulosta
  if (mjono1 > "Apua") cout << "1 suurempi kuin Apua" << endl;  // 1 suurempi ku
  if ("Apua" > mjono2) cout << "Apua suurempi kuin 2" << endl;  // ei tulosta

  if (mjono1 <= mjono2) cout << "1 pienempi tai yhtasuuri kuin 2" << endl;
  if (mjono1 >= mjono2) cout << "1 suurempi tai yhtasuuri kuin 2" << endl;
// ja vastaavat vakiomerkkijonoilla!  EI onnistu, koska vakiomerkkijonot ovat
                                   // OSOITTIMIA!
  mjono1.erase(4,2);
  cout << mjono1 << endl; // Ensiainen
  mjono1.insert(4,"mm");
  cout << mjono1 << endl; // Ensimmainen
  return 0;
}

