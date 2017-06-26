import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class Patch {
    
    public Patch(String file, TileSet ts) {

	System.out.println("Patching '" + file + "'.");
	ts.patch(file);

    }

}
