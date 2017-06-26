import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

public class TileArtist extends JPanel implements MouseListener{
    TileSet tileset;
    TileEditor tileeditor;

    Color darkgray;
    Color lightgray;
  


    public TileArtist(TileSet ts, TileEditor te) {
	super();
	tileset = ts;
	tileeditor = te;
	darkgray = new Color((float).3, (float).3, (float).3);
	lightgray = new Color((float).6, (float).6, (float).6);

	addMouseListener(this);

    }


    public void paint(Graphics g){
	super.paint(g);
	Graphics2D g2d = (Graphics2D) g;

	drawgrid(g2d);

	drawtiles(g2d);
	

    }


    public void drawgrid(Graphics2D g2d) {
	for (int i = 16; i < 170; i += 17) 
	    g2d.drawLine(i, 0, i, 1100) ;
	for (int i = 16; i < 1100; i += 17) 
	    g2d.drawLine(0, i, 170, i); 
    }

    public void drawtiles(Graphics2D g2d) {
	if (tileset.tiles == null) return;

	for (int i = 0; i < tileset.tiles.size(); i ++) 
	    drawtile(g2d, (Tile)tileset.tiles.elementAt(i), 
		     i % 8 * 17, i / 8 * 17);
    }



    public void drawtile(Graphics g2d, Tile t, int x, int y) {
	for (int iy = 0; iy < 8; iy ++) 
	    for (int ix = 0; ix < 8; ix ++) {
		if (t.tile[ix + iy * 8] == 0) g2d.setColor(Color.white);
		else if (t.tile[ix + iy * 8] == 1) g2d.setColor(lightgray);
		else if (t.tile[ix + iy * 8] == 2) g2d.setColor(darkgray);
		else if (t.tile[ix + iy * 8] == 3) g2d.setColor(Color.black);
		else  g2d.setColor(Color.red);


		g2d.fillRect(x + ix*2, y + iy*2,
			     2,2);


	    }
    
    }

    public void mouseClicked(MouseEvent e) {


    }

    public void mouseEntered(MouseEvent e) {

    }

    public void mouseExited(MouseEvent e) {


    }
    
    public void mousePressed(MouseEvent e) {
	int tile = e.getX()/17 + e.getY()/17 * 8;
	if (tile < tileset.tiles.size()) 
	    tileeditor.tileclicked(tile);


    }

    public void mouseReleased(MouseEvent e) {

    }

}
