
/**
 * Projet : InterfaceCCCMS
 * Objet : Window1
 *
 * Date : 01/06/2010 09:56:04
 * Version de WDJAVA.DLL  : 15.00Hc
 *
 * Ce fichier a �t� g�n�r� par WinDev lors de la premi�re exportation en Java du projet.
 * Ce fichier ne sera plus modifi� par WinDev lors des prochaines exportations.
 *
 * La classe <Window1> vous permet de personnaliser le fonctionnement de la fen�tre WinDev <Window1>.
 * Les modifications effectu�es dans le projet WinDev seront automatiquement prises en compte au moment de l'exportation en Java du projet.
 * Toutefois si vous supprimez un champ dans la fen�tre WinDev, vous devrez en supprimer les r�f�rences dans le code de la classe <Fen_caract>.
 *
 * De nombreuses classes et m�thodes sont disponibles pour manipuler vos champs et vos fen�tres en Java.
 * Consultez la javadoc fournie avec WinDev pour les d�couvrir.
 *
 *
 * Pour acc�der aux champs de la fen�tre, utilisez tout simplement le nom du champ tel qu'il est d�fini dans le projet WinDev.
 * ex : Saisie1.setValue("MonTexte");
 *
 * Pour ouvrir une fen�tre, utiliser la m�thode statique openWindow(String strName, boolean bModal) de la classe du projet : 
 * Ex : MonProjet.openWindow("Fen�tre2", false);
 * Attention : le nom de la fen�tre est sensible � la casse et � l'accentuation. 
 *
 * Pour acc�der aux champs ou aux m�thodes d'une autre fen�tre, utiliser l'accesseur correspondant dans la classe du projet : get<Nom de la fen�tre>
 * ex : MonProjet.getFenetre2().setTitle("Titre de la fen�tre");
 * Rappel : La fen�tre doit d�j� �tre ouverte pour pouvoir appeler ses m�thodes.
 *
 * Pour acc�der aux champs d'une autre fen�tre, utiliser la syntaxe suivante : 
 * ex : MonProjet.getFenetre2().Saisie1.setValue("MonTexte");
 */
import fr.pcsoft.wdjava.ext.ui.IWDWindow;
import fr.pcsoft.wdjava.ext.ui.event.IWDActionListener;
import fr.pcsoft.wdjava.ext.ui.event.WDActionEvent;

public class Window1 extends Window1Adapter {

    public Window1(IWDWindow window) {
        super(window); // code g�n�r� par WinDev - NE PAS SUPPRIMER !

        BtnNewCrisis.addActionListener(new IWDActionListener() {

            public void actionPerformed(WDActionEvent arg0) {
                setPlane(2);
            }
        });
    }
}
