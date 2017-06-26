import java.awt.*;
import javax.swing.*;
import java.awt.event.*;



public class MainWindow implements ActionListener{
    /* Main */
    public static final void main(String [] args) {
	MainWindow mw = new MainWindow();
	mw.go();
    }

    /* Fields */

    private JFrame mainframe;

    public TileEditor te;

    public TileSetView tilesetview;

    //public TileSetView and TileEditor

    /* Constructors */
    public MainWindow(){
	


    }

    public void go(){
	mainframe = new JFrame("Merchandise");

	//mainframe.getContentPane().setLayout(new GridLayout(1,2));
	mainframe.setSize(new Dimension(500, 300));
	
	JSplitPane js = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);

	mainframe.getContentPane().add(js);

	js.setDividerLocation(138);

	//mainframe.getConentPane(). add tileset and tileeditor 
	TileSet ts = new TileSet();

	te = new TileEditor(ts, this);

	JPanel jp = new JPanel();
	jp.setLayout(new BorderLayout());
	jp.add(te, BorderLayout.CENTER);

	JButton jb = new JButton("Clear");
	jp.add(jb, BorderLayout.SOUTH);

	jb.addActionListener(this);
	

	mainframe.addKeyListener(te);

	tilesetview = new TileSetView(ts, te, mainframe, this);

        js.setTopComponent(tilesetview);
	js.setBottomComponent(jp);

	mainframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); 

	mainframe.show();
    }

    public void actionPerformed(ActionEvent e) {
	te.clearAll();

    }

    public void redraw() {
	mainframe.repaint();
    }

}
