import java.util.*;

public interface FindInFile {

    /* tofind is a list of byte[], which must be
       all of the same length. */
    public LinkedList find(String file, LinkedList tofind);

}
