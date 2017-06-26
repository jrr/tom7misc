import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class FindDialog extends JDialog implements ActionListener {
    
    JButton jbutton;
    JTextPane jtextpane;
    TileSetView tilesetview;

    JScrollPane scrolltext;

    JTextField romname;

    public FindDialog(TileSetView ts, String text) {
	super();

	tilesetview = ts;

	jbutton = new JButton("Find Tiles");

	jbutton.addActionListener(this);

	jtextpane = new JTextPane();

	romname = new JTextField();

	scrolltext = new JScrollPane(jtextpane);

	romname.setText(tilesetview.lastrom);

	getContentPane().setLayout(new BorderLayout());

	getContentPane().add(romname, BorderLayout.NORTH);
	getContentPane().add(jbutton, BorderLayout.SOUTH);
	getContentPane().add(scrolltext, BorderLayout.CENTER);

	jtextpane.setText(text);
	
	setSize(new Dimension(500, 500));
	setLocation(100,100);

	setVisible(true);

    }


    public void actionPerformed(ActionEvent e) {
	Object o = e.getSource();

	if (o == jbutton) {
	    tilesetview.find(romname.getText(), jtextpane.getText());

	    tilesetview.lastrom = romname.getText();
	    
	    dispose();
	}
    }


}
