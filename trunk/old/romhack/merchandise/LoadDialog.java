import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class LoadDialog extends JDialog implements ActionListener{
    
    JButton jbutton;
    JTextPane jtextpane;
    TileSetView tilesetview;

    public LoadDialog(TileSetView ts, String text, JFrame jf) {       
	super(jf, "Tiles as Strings", true);

	tilesetview = ts;

	jbutton = new JButton("Send Tiles");

	jbutton.addActionListener(this);

	jtextpane = new JTextPane();

	JScrollPane jsp = new JScrollPane(jtextpane);
	//	jsp.add(jtextpane);

	getContentPane().setLayout(new BorderLayout());
	
	
	getContentPane().add(jbutton, BorderLayout.SOUTH);
	getContentPane().add(jsp, BorderLayout.CENTER);

	
	jtextpane.setText(text);
	
	setSize(new Dimension(500, 500));
	setLocation(100,100);

	setVisible(true);


    }


    public void actionPerformed(ActionEvent e) {
	tilesetview.update(jtextpane.getText());
	
	dispose();

    }


}
