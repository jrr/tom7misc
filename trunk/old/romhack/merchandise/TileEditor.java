import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

public class TileEditor extends JPanel implements MouseListener, KeyListener, MouseMotionListener {

    TileSet tileset;

    int []drawingarea;

    
    int mode = -1; /* -1: no mode
		 0 : add tile
		 1 : draw tile
	      */
    int pencolor = -1;

    int pentile;

    boolean grid = true;

    Color darkgray;
    Color lightgray;
    Color lightgreen;
    Color gridcolor;

    MainWindow mainwindow;

    public TileEditor(TileSet ts, MainWindow mw) {
	mainwindow = mw;
	tileset = ts;
	drawingarea = new int[64 * 64];
	for (int i = 0; i < 64 * 64; i ++) 
	    drawingarea[i] = -1;
	darkgray = new Color((float).3, (float).3, (float).3);
	lightgray = new Color((float).6, (float).6, (float).6);
	lightgreen = new Color((float).6, (float)1.0, (float).6);
	gridcolor = new Color((float).2, (float).2, (float).6);
	addMouseListener(this);
	addMouseMotionListener(this);
    }

    public void tileclicked(int clicked) {
	mode = 0;
	pentile = clicked;
	System.out.println(clicked);
    }

    public void removetiles() {
	for (int y = 0; y < 64; y++)
	    for(int x = 0; x < 64; x++) 
		drawingarea[y * 64 + x] = -1;

    }

    public void clearAll() {
	for (int y = 0; y < 64; y ++)
	    for (int x = 0; x < 64; x ++) {
		int tile = drawingarea[y * 64 + x];
		if (tile != -1)
		    ((Tile)tileset.tiles.elementAt(drawingarea[y * 64 + x])).clear();
	    }
	mainwindow.redraw();
		

    }


    public void paint(Graphics g){
	super.paint(g);
	
	Graphics2D g2d = (Graphics2D) g;



	if (tileset.tiles == null) return;
	
	for (int y = 0; y < 64; y ++)
	    for (int x = 0; x < 64; x ++) {
		int tile = drawingarea[y * 64 + x];
		if (tile != -1)
		    drawtile(g2d, (Tile)tileset.tiles.elementAt(tile), x * 48, y * 48);
	    }      

	if (grid) drawgrid(g2d);

    }

    public void drawgrid(Graphics2D g2d) {
	g2d.setColor(gridcolor);
	
	for (int y = 1; y < 64; y ++) 
	    g2d.drawLine(0, y * 48, 1100, y * 48);

	for (int x = 1; x < 64; x ++) 
	    g2d.drawLine(x * 48, 0, x * 48, 1100);


	



    }

    public void drawtile(Graphics2D g2d, Tile t, int x, int y) {
	for (int iy = 0; iy < 8; iy ++) 
	    for (int ix = 0; ix < 8; ix ++) {
		if (t.tile[ix + iy * 8] == 0) g2d.setColor(Color.white);
		else if (t.tile[ix + iy * 8] == 1) g2d.setColor(lightgray);
		else if (t.tile[ix + iy * 8] == 2) g2d.setColor(darkgray);
		else if (t.tile[ix + iy * 8] == 3) g2d.setColor(Color.black);
		else  g2d.setColor(Color.red);


		g2d.fillRect(x + ix*6, y + iy*6,
			     6,6);


	    }
    
    }

    public void mouseClicked(MouseEvent e) {

    }

    public void mouseEntered(MouseEvent e) {

    }

    public void mouseExited(MouseEvent e) {

    }

    public void mousePressed(MouseEvent e) {
	if (mode == 0) {
	    int tile = e.getX() / 48 + (e.getY() / 48) * 64; 
	    drawingarea[tile] = pentile; 
	    //	    mainwindow.redraw();
	    repaint();
	}

	/* TEE HEE */
	mouseDragged(e);

    }



    public void mouseReleased(MouseEvent e) {
	mainwindow.redraw();

    }

    /* Mouse Motion */
    public  void mouseDragged(MouseEvent e) {
	if (mode != 1) return;

	if ((e.getX() < 0) || (e.getY() < 0)) return;

	int tile = e.getX() / 48 + (e.getY() / 48) * 64; 
	int xoff = (e.getX() % 48) / 6;
	int yoff = (e.getY() % 48) / 6;

	if (drawingarea[tile] != -1) {
	    Tile t = (Tile)tileset.tiles.elementAt(drawingarea[tile]);
	    if (t.tile[xoff + yoff * 8] != pencolor){
		t.tile[xoff + yoff * 8] = pencolor;
		repaint();
	    }
	}

    }

    /* Mouse Motion */
    public void mouseMoved(MouseEvent e) {
	
    }

    public void keyPressed(KeyEvent e) {
	if (e.getKeyChar() == '1') {
	    mode = 1;
	    pencolor = 0;
	}
	if (e.getKeyChar() == '2') {
	    mode = 1;
	    pencolor = 1;
	}
	if (e.getKeyChar() == '3') {
	    mode = 1;
	    pencolor = 2;
	}
	if (e.getKeyChar() == '4') {
	    mode = 1;
	    pencolor = 3;
	}
	if (e.getKeyChar() == '`') {
	    grid = !grid;
	    mainwindow.redraw();
	}

    

    }

    public void keyReleased(KeyEvent e){
    }

    public void keyTyped(KeyEvent e) {

    }

}
