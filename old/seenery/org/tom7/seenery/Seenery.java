package org.tom7.seenery;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.MessageFormat;
import java.util.Date;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Vector;
import java.util.Arrays;
import java.util.Comparator;
import java.lang.*;
import java.security.MessageDigest;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.FileTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

/* TODO: ktable.sourceforge.net gives a much fancier table,
 * which could lead to better rendering (colors; show comment 
 * below filename, etc.)
 */

public class Seenery {
	private static ResourceBundle resourceBundle = ResourceBundle
			.getBundle("seenery_resources");

	/* UI elements */
	private Display display;

	private Shell shell;

	private ToolBar toolBar;

	private Label numObjectsLabel;

	private Label diskSpaceLabel;

	private File currentDirectory = null;

	private boolean initial = true;

	/* Drag and drop optimizations */
	private boolean isDragging = false; // if this app is dragging

	private boolean isDropping = false; // if this app is dropping

	private File[] processedDropFiles = null; // so Drag only deletes what it

	// needs to

	private File[] deferredRefreshFiles = null; // to defer notifyRefreshFiles

	// while we do DND

	private boolean deferredRefreshRequested = false; // to defer

	// notifyRefreshFiles
	// while we do DND

	private ProgressDialog progressDialog = null; // progress dialog for

	// locally-initiated
	// operations

	/* Tree view */
	private IconCache iconcache = new IconCache();

	/* Table view */

	// File: File+info associated with table row
	private static final String TABLEITEMDATA_INFO = "TableItem.info";

	// currently visible directory; associated with whole table
	private static final String TABLEDATA_DIR = "Table.dir";

	private static final int[] tableWidths = new int[] { 150, 60, 75, 100, 100 };

	private final String[] tableTitles = new String[] { "name", "type", "size",
			"modified", "hash" };

	private Table table;

	private Label tableContentsOfLabel;

	/* Table update worker */
	// Control data
	/* Lock for all worker control data and state */
	private final Object workerLock = new Object();

	private volatile Thread workerThread = null;

	/* Exit after current cycle? */
	private volatile boolean workerStopped = false;

	/* Stop working? */
	private volatile boolean workerCancelled = false;

	/*
	 * these are the infos we should process, starting from workerInfoIdx and
	 * going to the length of the array.
	 */
	private volatile Vector<FileInfo> workerInfos = null;

	private volatile int workerInfoIdx = 0;

	// Worker state information -- this is what gets synchronized by an update
	private volatile File workerStateDir = null;

	// State information to use for the next cycle
	private volatile File workerNextDir = null;

	/* Simulate only flag */
	/* XXX bye */
	// when true, disables actual filesystem manipulations and outputs results
	// to standard out
	private boolean simulateOnly = true;

	/**
	 * Runs main program.
	 */
	public static void main(String[] args) {
		FileDB.init();
		File startdir;

		try {
			startdir = new File(".").getCanonicalFile();
		} catch (IOException e) {
			// Should be impossible.
			e.printStackTrace();
			throw new Error();
		}

		/*
		 * XXX handle errors?? XXX is args[0] the program name, like C?
		 */
		if (args.length >= 2)
			startdir = new File(args[1]);

		Display display = new Display();
		Seenery application = new Seenery();
		Shell shell = application.open(display, startdir);
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		application.close();
		display.dispose();
	}

	/**
	 * Opens the main program.
	 */
	public Shell open(Display display, File startdir) {
		// Create the window
		this.display = display;
		iconcache.initResources(display);
		shell = new Shell();
		createShellContents();
		// why??
		notifyRefreshFiles(null);
		notifySelectedDirectory(startdir);
		shell.open();
		return shell;
	}

	/**
	 * Closes the main program.
	 */
	void close() {
		workerStop();
		iconcache.freeResources();
	}

	/**
	 * Construct the UI
	 * 
	 * @param container
	 *            the ShellContainer managing the Shell we are rendering inside
	 */
	private void createShellContents() {
		shell.setText("Seenery");
		shell.setImage(iconcache.stockImages[iconcache.shellIcon]);
		Menu bar = new Menu(shell, SWT.BAR);
		shell.setMenuBar(bar);
		createFileMenu(bar);
		createHelpMenu(bar);

		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 3;
		gridLayout.marginHeight = gridLayout.marginWidth = 0;
		shell.setLayout(gridLayout);

		GridData gridData = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
		gridData.widthHint = 185;
		// createComboView(shell, gridData);
		gridData = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
		gridData.horizontalSpan = 2;
		createToolBar(shell, gridData);

		SashForm sashForm = new SashForm(shell, SWT.NONE | SWT.SMOOTH);
		sashForm.setOrientation(SWT.HORIZONTAL);
		gridData = new GridData(GridData.FILL_HORIZONTAL
				| GridData.FILL_VERTICAL);
		gridData.horizontalSpan = 3;
		sashForm.setLayoutData(gridData);
		// XXX creates empty thing here--should put something there !
		Composite composite = new Composite(sashForm, SWT.NONE);
		createTableView(sashForm);
		sashForm.setWeights(new int[] { 2, 5 });

		numObjectsLabel = new Label(shell, SWT.BORDER);
		gridData = new GridData(GridData.FILL_HORIZONTAL
				| GridData.VERTICAL_ALIGN_FILL);
		gridData.widthHint = 185;
		numObjectsLabel.setLayoutData(gridData);

		diskSpaceLabel = new Label(shell, SWT.BORDER);
		gridData = new GridData(GridData.FILL_HORIZONTAL
				| GridData.VERTICAL_ALIGN_FILL);
		gridData.horizontalSpan = 2;
		diskSpaceLabel.setLayoutData(gridData);
	}

	/**
	 * Creates the File Menu.
	 * 
	 * @param parent
	 *            the parent menu
	 */
	private void createFileMenu(Menu parent) {
		Menu menu = new Menu(parent);
		MenuItem header = new MenuItem(parent, SWT.CASCADE);
		header.setText("File");
		header.setMenu(menu);

		final MenuItem simulateItem = new MenuItem(menu, SWT.CHECK);
		simulateItem.setText("Simulate Only");
		simulateItem.setSelection(simulateOnly);
		simulateItem.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				simulateOnly = simulateItem.getSelection();
			}
		});

		MenuItem item = new MenuItem(menu, SWT.PUSH);
		item.setText("Close");
		item.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				shell.close();
			}
		});
	}

	/**
	 * Creates the Help Menu.
	 * 
	 * @param parent
	 *            the parent menu
	 */
	private void createHelpMenu(Menu parent) {
		Menu menu = new Menu(parent);
		MenuItem header = new MenuItem(parent, SWT.CASCADE);
		header.setText("Help");
		header.setMenu(menu);

		MenuItem item = new MenuItem(menu, SWT.PUSH);
		item.setText("About");
		item.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				MessageBox box = new MessageBox(shell, SWT.ICON_INFORMATION
						| SWT.OK);
				box.setText("About");
				box.setMessage("About"); // , 0);
				// new Object[] { System.getProperty("os.name") });
				box.open();
			}
		});
	}

	/**
	 * Creates the toolbar
	 * 
	 * @param shell
	 *            the shell on which to attach the toolbar
	 * @param layoutData
	 *            the layout data
	 */
	private void createToolBar(final Shell shell, Object layoutData) {
		toolBar = new ToolBar(shell, SWT.NULL);
		toolBar.setLayoutData(layoutData);
		ToolItem item = new ToolItem(toolBar, SWT.SEPARATOR);
		item = new ToolItem(toolBar, SWT.PUSH);
		item.setImage(iconcache.stockImages[iconcache.cmdParent]);
		item.setToolTipText("Parent");
		item.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				doParent();
			}
		});
		item = new ToolItem(toolBar, SWT.PUSH);
		item.setImage(iconcache.stockImages[iconcache.cmdRefresh]);
		item.setToolTipText("Refresh");
		item.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				doRefresh();
			}
		});
		SelectionAdapter unimplementedListener = new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				MessageBox box = new MessageBox(shell, SWT.ICON_INFORMATION
						| SWT.OK);
				box.setText("NotImplemented");
				box.setMessage("ActionNotImplemented");
				box.open();
			}
		};

		item = new ToolItem(toolBar, SWT.SEPARATOR);
		item = new ToolItem(toolBar, SWT.PUSH);
		item.setImage(iconcache.stockImages[iconcache.cmdCut]);
		item.setToolTipText("Cut");
		item.addSelectionListener(unimplementedListener);

		item = new ToolItem(toolBar, SWT.PUSH);
		item.setImage(iconcache.stockImages[iconcache.cmdCopy]);
		item.setToolTipText("Copy");
		item.addSelectionListener(unimplementedListener);

		item = new ToolItem(toolBar, SWT.PUSH);
		item.setImage(iconcache.stockImages[iconcache.cmdPaste]);
		item.setToolTipText("Paste");
		item.addSelectionListener(unimplementedListener);

		item = new ToolItem(toolBar, SWT.SEPARATOR);

		item = new ToolItem(toolBar, SWT.PUSH);
		item.setImage(iconcache.stockImages[iconcache.cmdDelete]);
		item.setToolTipText("Delete");
		item.addSelectionListener(unimplementedListener);

		item = new ToolItem(toolBar, SWT.PUSH);
		item.setImage(iconcache.stockImages[iconcache.cmdRename]);
		item.setToolTipText("Rename");
		item.addSelectionListener(unimplementedListener);

		item = new ToolItem(toolBar, SWT.SEPARATOR);
		item = new ToolItem(toolBar, SWT.PUSH);
		item.setImage(iconcache.stockImages[iconcache.cmdSearch]);
		item.setToolTipText("Search");
		item.addSelectionListener(unimplementedListener);

	}

	/**
	 * Creates the file details table.
	 * 
	 * @param parent
	 *            the parent control
	 */
	private void createTableView(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 1;
		gridLayout.marginHeight = gridLayout.marginWidth = 2;
		gridLayout.horizontalSpacing = gridLayout.verticalSpacing = 0;
		composite.setLayout(gridLayout);
		tableContentsOfLabel = new Label(composite, SWT.BORDER);
		tableContentsOfLabel.setLayoutData(new GridData(
				GridData.FILL_HORIZONTAL | GridData.VERTICAL_ALIGN_FILL));

		table = new Table(composite, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL
				| SWT.MULTI | SWT.FULL_SELECTION);
		table.setLayoutData(new GridData(GridData.FILL_HORIZONTAL
				| GridData.FILL_VERTICAL));

		for (int i = 0; i < tableTitles.length; ++i) {
			TableColumn column = new TableColumn(table, SWT.NONE);
			column.setText(tableTitles[i]);
			column.setWidth(tableWidths[i]);
		}
		table.setHeaderVisible(true);
		table.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				notifySelectedFiles(getSelectedFiles());
			}

			public void widgetDefaultSelected(SelectionEvent event) {
				doDefaultFileAction(getSelectedFiles());
			}

			private FileInfo[] getSelectedFiles() {
				final TableItem[] items = table.getSelection();
				final FileInfo[] files = new FileInfo[items.length];

				for (int i = 0; i < items.length; ++i) {
					files[i] = (FileInfo) items[i].getData(TABLEITEMDATA_INFO);
				}
				return files;
			}
		});

		table.addKeyListener(new KeyAdapter() {
			public void keyPressed(KeyEvent e) {
				handlekey(e);
			}
		});

		// createTableDragSource(table);
		// createTableDropTarget(table);
	}

	/**
	 * Handles a keypress (from the table listener).
	 */
	private void handlekey(KeyEvent e) {
		/*
		 * In order to stop these keys from being passed on, we can set the
		 * e.doit field to false. This is extremely nasty, but it is the swt
		 * way, apparently.
		 */
		/*
		 * Other option: "You could add an event filter to the display
		 * (Display.addFilter())"..
		 */
		switch (e.character) {
		case '1':
		case '!':
			e.doit = false;
			applytoselected(new FileAction() {
				public void act(FileInfo fi, FileData fd) {
					fd.rating = FileData.EXCLAIM;
					System.out.println("seen " + fi.display);
				}
			});
			break;
		/* XXX also double-exclaim; how? */
		case '`': {
			/* FIXME prompt for comment */
			e.doit = false;
			final String comment = "XXX prompt";
			applytoselected(new FileAction() {
				public void act(FileInfo fi, FileData fd) {
					/* XXX do it */
					System.out.println("comment " + fi.display + " : "
							+ comment);
				}
			});
		}
		case '+':
		case '=':
			e.doit = false;
			applytoselected(new FileAction() {
				public void act(FileInfo fi, FileData fd) {
					fd.seen = true;
					System.out.println("seen " + fi.display);
				}
			});
			break;
		case '_':
		case '-':
			e.doit = false;
			applytoselected(new FileAction() {
				public void act(FileInfo fi, FileData fd) {
					fd.seen = false;
					System.out.println("unseen " + fi.display);
				}
			});
			break;
		default:
			break;
		}
		/* XXX also backspace... */
	}

	private void applytoselected(FileAction fa) {
		TableItem ti[] = table.getSelection();
		for (int i = 0; i < ti.length; i++) {
			FileInfo fi = (FileInfo) ti[i].getData(TABLEITEMDATA_INFO);
			if (!fi.isLoaded()) {
				/* PERF in GUI thread? well, it's not THAT slow... */
				fi.fullDetails();
			}

			FileData fd = fi.getDataOrDefault();

			if (fd != null) {
				System.out.println("do file action..." + fi.display);
				fa.act(fi, fd);

				/* write to disk? */
				/* PERF -- should avoid writing if we didn't change it */
				/*
				 * PERF -- should avoid writing if it is the default (instead
				 * delete from db?)
				 */
				fi.saveData();
				/* XXX for correctness, need to update anything else with the
				 * same hash...
				 */
				updatestrings(ti[i]);
			} else {
				System.out.println("no data available...");
			}
		}
	}

	/**
	 * Creates the Drag & Drop DragSource for items being dragged from the
	 * table.
	 * 
	 * @return the DragSource for the table
	 */
	private DragSource createTableDragSource(final Table table) {
		DragSource dragSource = new DragSource(table, DND.DROP_MOVE
				| DND.DROP_COPY);
		dragSource.setTransfer(new Transfer[] { FileTransfer.getInstance() });
		dragSource.addDragListener(new DragSourceListener() {
			TableItem[] dndSelection = null;

			String[] sourceNames = null;

			public void dragStart(DragSourceEvent event) {
				dndSelection = table.getSelection();
				sourceNames = null;
				event.doit = dndSelection.length > 0;
				isDragging = true;
			}

			public void dragFinished(DragSourceEvent event) {
				dragSourceHandleDragFinished(event, sourceNames);
				dndSelection = null;
				sourceNames = null;
				isDragging = false;
				handleDeferredRefresh();
			}

			public void dragSetData(DragSourceEvent event) {
				if (dndSelection == null || dndSelection.length == 0)
					return;
				if (!FileTransfer.getInstance().isSupportedType(event.dataType))
					return;

				sourceNames = new String[dndSelection.length];
				for (int i = 0; i < dndSelection.length; i++) {
					FileInfo fi = (FileInfo) dndSelection[i]
							.getData(TABLEITEMDATA_INFO);
					sourceNames[i] = fi.file.getAbsolutePath();
				}
				event.data = sourceNames;
			}
		});
		return dragSource;
	}

	/**
	 * Creates the Drag & Drop DropTarget for items being dropped onto the
	 * table.
	 * 
	 * @return the DropTarget for the table
	 */
	private DropTarget createTableDropTarget(final Table table) {
		DropTarget dropTarget = new DropTarget(table, DND.DROP_MOVE
				| DND.DROP_COPY);
		dropTarget.setTransfer(new Transfer[] { FileTransfer.getInstance() });
		dropTarget.addDropListener(new DropTargetAdapter() {
			public void dragEnter(DropTargetEvent event) {
				isDropping = true;
			}

			public void dragLeave(DropTargetEvent event) {
				isDropping = false;
				handleDeferredRefresh();
			}

			public void dragOver(DropTargetEvent event) {
				dropTargetValidate(event, getTargetFile(event));
				event.feedback |= DND.FEEDBACK_EXPAND | DND.FEEDBACK_SCROLL;
			}

			public void drop(DropTargetEvent event) {
				File targetFile = getTargetFile(event);
				if (dropTargetValidate(event, targetFile))
					dropTargetHandleDrop(event, targetFile);
			}

			private File getTargetFile(DropTargetEvent event) {
				// Determine the target File for the drop
				TableItem item = table.getItem(table.toControl(new Point(
						event.x, event.y)));
				File targetFile = null;
				if (item == null) {
					// We are over an unoccupied area of the table.
					// If it is a COPY, we can use the table's root file.
					if (event.detail == DND.DROP_COPY) {
						targetFile = (File) table.getData(TABLEDATA_DIR);
					}
				} else {
					// We are over a particular item in the table, use the
					// item's file
					FileInfo fi = (FileInfo) item.getData(TABLEITEMDATA_INFO);
					targetFile = fi.file;
				}
				return targetFile;
			}
		});
		return dropTarget;
	}

	/**
	 * Notifies the application components that a new current directory has been
	 * selected
	 * 
	 * @param dir
	 *            the directory that was selected, null is ignored
	 */
	void notifySelectedDirectory(File dir) {
		if (dir == null)
			return;
		if (currentDirectory != null && dir.equals(currentDirectory))
			return;
		currentDirectory = dir;
		notifySelectedFiles(null);

		/*
		 * Shell: Sets the title to indicate the selected directory
		 */
		shell.setText("Seenery : " + currentDirectory.getPath());

		/*
		 * Table view: Displays the contents of the selected directory.
		 */
		workerUpdate(dir, false);

	}

	/**
	 * Notifies the application components that files have been selected
	 * 
	 * @param files
	 *            the files that were selected, null or empty array indicates no
	 *            active selection
	 */
	void notifySelectedFiles(FileInfo[] files) {
		/*
		 * Details: Update the details that are visible on screen.
		 */
		if ((files != null) && (files.length != 0)) {
			numObjectsLabel.setText("Selected Files: " + files.length);
			long fileSize = 0L;
			for (int i = 0; i < files.length; ++i) {
				fileSize += files[i].file.length();
			}
			diskSpaceLabel.setText("Size: " + fileSize);
		} else {
			// No files selected
			diskSpaceLabel.setText("");
			if (currentDirectory != null) {
				int numObjects = getDirectoryList(currentDirectory).length;
				numObjectsLabel.setText("Number of objects: " + numObjects);
			} else {
				numObjectsLabel.setText("");
			}
		}
	}

	/**
	 * Notifies the application components that files must be refreshed
	 * 
	 * @param files
	 *            the files that need refreshing, empty array is a no-op, null
	 *            refreshes all
	 */
	void notifyRefreshFiles(File[] files) {
		if (files != null && files.length == 0)
			return;

		if ((deferredRefreshRequested) && (deferredRefreshFiles != null)
				&& (files != null)) {
			// merge requests
			File[] newRequest = new File[deferredRefreshFiles.length
					+ files.length];
			System.arraycopy(deferredRefreshFiles, 0, newRequest, 0,
					deferredRefreshFiles.length);
			System.arraycopy(files, 0, newRequest, deferredRefreshFiles.length,
					files.length);
			deferredRefreshFiles = newRequest;
		} else {
			deferredRefreshFiles = files;
			deferredRefreshRequested = true;
		}
		handleDeferredRefresh();
	}

	/**
	 * Handles deferred Refresh notifications (due to Drag & Drop)
	 */
	void handleDeferredRefresh() {
		if (isDragging || isDropping || !deferredRefreshRequested)
			return;
		if (progressDialog != null) {
			progressDialog.close();
			progressDialog = null;
		}

		deferredRefreshRequested = false;
		File[] files = deferredRefreshFiles;
		deferredRefreshFiles = null;

		shell.setCursor(iconcache.stockCursors[iconcache.cursorWait]);

		/*
		 * Table view: Refreshes information about any files in the list and
		 * their children.
		 */
		boolean refreshTable = false;
		if (files != null) {
			for (int i = 0; i < files.length; ++i) {
				final File file = files[i];
				if (file.equals(currentDirectory)) {
					refreshTable = true;
					break;
				}
				File parentFile = file.getParentFile();
				if ((parentFile != null)
						&& (parentFile.equals(currentDirectory))) {
					refreshTable = true;
					break;
				}
			}
		} else
			refreshTable = true;
		if (refreshTable)
			workerUpdate(currentDirectory, true);

		// Remind everyone where we are in the filesystem
		final File dir = currentDirectory;
		currentDirectory = null;
		notifySelectedDirectory(dir);

		shell.setCursor(iconcache.stockCursors[iconcache.cursorDefault]);
	}

	/**
	 * Performs the default action on a set of files.
	 * 
	 * @param files
	 *            the array of files to process
	 */
	void doDefaultFileAction(FileInfo[] files) {
		// only uses the 1st file (for now)
		if (files.length == 0)
			return;
		final File file = files[0].file;

		if (file.isDirectory()) {
			notifySelectedDirectory(file);
		} else {
			final String fileName = file.getAbsolutePath();
			if (!Program.launch(fileName)) {
				MessageBox dialog = new MessageBox(shell, SWT.ICON_ERROR
						| SWT.OK);
				dialog.setMessage("Could not launch " + fileName);
				dialog.setText(shell.getText());
				dialog.open();
			}
		}
	}

	/**
	 * Navigates to the parent directory
	 */
	void doParent() {
		if (currentDirectory == null)
			return;
		File parentDirectory = currentDirectory.getParentFile();
		notifySelectedDirectory(parentDirectory);
	}

	/**
	 * Performs a refresh
	 */
	void doRefresh() {
		notifyRefreshFiles(null);
	}

	/**
	 * Validates a drop target as a candidate for a drop operation.
	 * <p>
	 * Used in dragOver() and dropAccept().<br>
	 * Note event.detail is set to DND.DROP_NONE by this method if the target is
	 * not valid.
	 * </p>
	 * 
	 * @param event
	 *            the DropTargetEvent to validate
	 * @param targetFile
	 *            the File representing the drop target location under
	 *            inspection, or null if none
	 */
	private boolean dropTargetValidate(DropTargetEvent event, File targetFile) {
		if (targetFile != null && targetFile.isDirectory()) {
			if (event.detail != DND.DROP_COPY && event.detail != DND.DROP_MOVE) {
				event.detail = DND.DROP_MOVE;
			}
		} else {
			event.detail = DND.DROP_NONE;
		}
		return event.detail != DND.DROP_NONE;
	}

	/**
	 * Handles a drop on a dropTarget.
	 * <p>
	 * Used in drop().<br>
	 * Note event.detail is modified by this method.
	 * </p>
	 * 
	 * @param event
	 *            the DropTargetEvent passed as parameter to the drop() method
	 * @param targetFile
	 *            the File representing the drop target location under
	 *            inspection, or null if none
	 */
	private void dropTargetHandleDrop(DropTargetEvent event, File targetFile) {
		// Get dropped data (an array of filenames)
		if (!dropTargetValidate(event, targetFile))
			return;
		final String[] sourceNames = (String[]) event.data;
		if (sourceNames == null)
			event.detail = DND.DROP_NONE;
		if (event.detail == DND.DROP_NONE)
			return;

		// Open progress dialog
		progressDialog = new ProgressDialog(shell,
				(event.detail == DND.DROP_MOVE) ? ProgressDialog.MOVE
						: ProgressDialog.COPY);
		progressDialog.setTotalWorkUnits(sourceNames.length);
		progressDialog.open();

		// Copy each file
		Vector<File> /* of File */processedFiles = new Vector<File>();
		for (int i = 0; (i < sourceNames.length)
				&& (!progressDialog.isCancelled()); i++) {
			final File source = new File(sourceNames[i]);
			final File dest = new File(targetFile, source.getName());
			if (source.equals(dest))
				continue; // ignore if in same location

			progressDialog.setDetailFile(source, ProgressDialog.COPY);
			while (!progressDialog.isCancelled()) {
				if (copyFileStructure(source, dest)) {
					processedFiles.add(source);
					break;
				} else if (!progressDialog.isCancelled()) {
					if (event.detail == DND.DROP_MOVE && (!isDragging)) {
						// It is not possible to notify an external drag source
						// that a drop
						// operation was only partially successful. This is
						// particularly a
						// problem for DROP_MOVE operations since unless the
						// source gets
						// DROP_NONE, it will delete the original data including
						// bits that
						// may not have been transferred successfully.
						MessageBox box = new MessageBox(shell, SWT.ICON_ERROR
								| SWT.RETRY | SWT.CANCEL);
						box.setText("Copy Failed.");
						box
								.setMessage("Copy Failed: " + source + " -> "
										+ dest);
						int button = box.open();
						if (button == SWT.CANCEL) {
							i = sourceNames.length;
							event.detail = DND.DROP_NONE;
							break;
						}
					} else {
						// We can recover gracefully from errors if the drag
						// source belongs
						// to this application since it will look at
						// processedDropFiles.
						MessageBox box = new MessageBox(shell, SWT.ICON_ERROR
								| SWT.ABORT | SWT.RETRY | SWT.IGNORE);
						box.setText("Copy Failed.");
						box
								.setMessage("Copy Failed: " + source + " -> "
										+ dest);
						int button = box.open();
						if (button == SWT.ABORT)
							i = sourceNames.length;
						if (button != SWT.RETRY)
							break;
					}
				}
				progressDialog.addProgress(1);
			}
		}
		if (isDragging) {
			// Remember exactly which files we processed
			processedDropFiles = ((File[]) processedFiles
					.toArray(new File[processedFiles.size()]));
		} else {
			progressDialog.close();
			progressDialog = null;
		}
		notifyRefreshFiles(new File[] { targetFile });
	}

	/**
	 * Handles the completion of a drag on a dragSource.
	 * <p>
	 * Used in dragFinished().<br>
	 * </p>
	 * 
	 * @param event
	 *            the DragSourceEvent passed as parameter to the dragFinished()
	 *            method
	 * @param sourceNames
	 *            the names of the files that were dragged (event.data is
	 *            invalid)
	 */
	private void dragSourceHandleDragFinished(DragSourceEvent event,
			String[] sourceNames) {
		if (sourceNames == null)
			return;
		if (event.detail != DND.DROP_MOVE)
			return;

		// Get array of files that were actually transferred
		final File[] sourceFiles;
		if (processedDropFiles != null) {
			sourceFiles = processedDropFiles;
		} else {
			sourceFiles = new File[sourceNames.length];
			for (int i = 0; i < sourceNames.length; ++i)
				sourceFiles[i] = new File(sourceNames[i]);
		}
		if (progressDialog == null)
			progressDialog = new ProgressDialog(shell, ProgressDialog.MOVE);
		progressDialog.setTotalWorkUnits(sourceFiles.length);
		progressDialog.setProgress(0);
		progressDialog.open();

		// Delete each file
		for (int i = 0; (i < sourceFiles.length)
				&& (!progressDialog.isCancelled()); i++) {
			final File source = sourceFiles[i];
			progressDialog.setDetailFile(source, ProgressDialog.DELETE);
			while (!progressDialog.isCancelled()) {
				if (deleteFileStructure(source)) {
					break;
				} else if (!progressDialog.isCancelled()) {
					MessageBox box = new MessageBox(shell, SWT.ICON_ERROR
							| SWT.ABORT | SWT.RETRY | SWT.IGNORE);
					box.setText("Delete failed.");
					box.setMessage("Could not delete: " + source);
					int button = box.open();
					if (button == SWT.ABORT)
						i = sourceNames.length;
					if (button == SWT.RETRY)
						break;
				}
			}
			progressDialog.addProgress(1);
		}
		notifyRefreshFiles(sourceFiles);
		progressDialog.close();
		progressDialog = null;
	}

	/**
	 * Gets a directory listing
	 * 
	 * @param file
	 *            the directory to be listed
	 * @return an array of files this directory contains, may be empty but not
	 *         null
	 */
	static File[] getDirectoryList(File file) {
		File[] list = file.listFiles();
		if (list == null)
			return new File[0];
		return list;
	}

	/**
	 * Copies a file or entire directory structure.
	 * 
	 * @param oldFile
	 *            the location of the old file or directory
	 * @param newFile
	 *            the location of the new file or directory
	 * @return true iff the operation succeeds without errors
	 */
	boolean copyFileStructure(File oldFile, File newFile) {
		if (oldFile == null || newFile == null)
			return false;

		// ensure that newFile is not a child of oldFile or a dupe
		File searchFile = newFile;
		do {
			if (oldFile.equals(searchFile))
				return false;
			searchFile = searchFile.getParentFile();
		} while (searchFile != null);

		if (oldFile.isDirectory()) {
			/*
			 * Copy a directory
			 */
			if (progressDialog != null) {
				progressDialog.setDetailFile(oldFile, ProgressDialog.COPY);
			}
			if (simulateOnly) {
				// System.out.println(getResourceString("simulate.DirectoriesCreated.text",
				// new Object[] { newFile.getPath() }));
			} else {
				if (!newFile.mkdirs())
					return false;
			}
			File[] subFiles = oldFile.listFiles();
			if (subFiles != null) {
				if (progressDialog != null) {
					progressDialog.addWorkUnits(subFiles.length);
				}
				for (int i = 0; i < subFiles.length; i++) {
					File oldSubFile = subFiles[i];
					File newSubFile = new File(newFile, oldSubFile.getName());
					if (!copyFileStructure(oldSubFile, newSubFile))
						return false;
					if (progressDialog != null) {
						progressDialog.addProgress(1);
						if (progressDialog.isCancelled())
							return false;
					}
				}
			}
		} else {
			/*
			 * Copy a file
			 */
			if (simulateOnly) {
				// System.out.println(getResourceString("simulate.CopyFromTo.text",
				// new Object[] { oldFile.getPath(), newFile.getPath() }));
			} else {
				FileReader in = null;
				FileWriter out = null;
				try {
					in = new FileReader(oldFile);
					out = new FileWriter(newFile);

					int count;
					while ((count = in.read()) != -1)
						out.write(count);
				} catch (FileNotFoundException e) {
					return false;
				} catch (IOException e) {
					return false;
				} finally {
					try {
						if (in != null)
							in.close();
						if (out != null)
							out.close();
					} catch (IOException e) {
						return false;
					}
				}
			}
		}
		return true;
	}

	/**
	 * Deletes a file or entire directory structure.
	 * 
	 * @param oldFile
	 *            the location of the old file or directory
	 * @return true iff the operation succeeds without errors
	 */
	boolean deleteFileStructure(File oldFile) {
		if (oldFile == null)
			return false;
		if (oldFile.isDirectory()) {
			/*
			 * Delete a directory
			 */
			if (progressDialog != null) {
				progressDialog.setDetailFile(oldFile, ProgressDialog.DELETE);
			}
			File[] subFiles = oldFile.listFiles();
			if (subFiles != null) {
				if (progressDialog != null) {
					progressDialog.addWorkUnits(subFiles.length);
				}
				for (int i = 0; i < subFiles.length; i++) {
					File oldSubFile = subFiles[i];
					if (!deleteFileStructure(oldSubFile))
						return false;
					if (progressDialog != null) {
						progressDialog.addProgress(1);
						if (progressDialog.isCancelled())
							return false;
					}
				}
			}
		}
		if (simulateOnly) {
			// System.out.println(getResourceString("simulate.Delete.text",
			// new Object[] { oldFile.getPath(), oldFile.getPath() }));
			return true;
		} else {
			return oldFile.delete();
		}
	}

	/*
	 * This worker updates the table with file information in the background.
	 * <p> Implementation notes: <ul> <li> It is designed such that it can be
	 * interrupted cleanly. <li> It uses asyncExec() in some places to ensure
	 * that SWT Widgets are manipulated in the right thread. Exclusive use of
	 * syncExec() would be inappropriate as it would require a pair of context
	 * switches between each table update operation. </ul> </p>
	 */

	/**
	 * Stops the worker and waits for it to terminate.
	 */
	void workerStop() {
		if (workerThread == null)
			return;
		synchronized (workerLock) {
			workerCancelled = true;
			workerStopped = true;
			workerLock.notifyAll();
		}
		while (workerThread != null) {
			if (!display.readAndDispatch())
				display.sleep();
		}
	}

	/**
	 * Notifies the worker that it should update itself with new data. Cancels
	 * any previous operation and begins a new one.
	 * 
	 * @param dir
	 *            the new base directory for the table, null is ignored
	 * @param force
	 *            if true causes a refresh even if the data are the same
	 */
	void workerUpdate(File dir, boolean force) {
		if (dir == null)
			return;
		if ((!force) && (workerNextDir != null) && (workerNextDir.equals(dir)))
			return;

		synchronized (workerLock) {
			workerNextDir = dir;
			workerStopped = false;
			workerCancelled = true;
			workerLock.notifyAll();
		}
		if (workerThread == null) {
			workerThread = new Thread(workerRunnable);
			workerThread.start();
		}
	}

	/**
	 * Manages the worker's thread
	 */
	private final Runnable workerRunnable = new Runnable() {
		public void run() {
			while (!workerStopped) {
				synchronized (workerLock) {
					workerCancelled = false;
					workerStateDir = workerNextDir;
				}
				workerExecute();
				synchronized (workerLock) {
					try {
						if ((!workerCancelled)
								&& (workerStateDir == workerNextDir))
							workerLock.wait();
					} catch (InterruptedException e) {
					}
				}
			}
			workerThread = null;
			// wake up UI thread in case it is in a modal loop awaiting thread
			// termination
			// (see workerStop())
			display.wake();
		}
	};

	private void sortlist(File[] l) {
		Arrays.sort(l, new Comparator<File>() {
			public int compare(File a, File b) {
				if (a.isDirectory() != b.isDirectory()) {
					if (a.isDirectory())
						return -1;
					else
						return 1;
				}
				return String.CASE_INSENSITIVE_ORDER.compare(a.getName(), b
						.getName());
			}
		});
	}

	/**
	 * Updates the table's contents
	 */
	private void workerExecute() {
		File[] dirlist;
		// Clear existing information
		workerInfos = new Vector<FileInfo>();
		workerInfoIdx = 0;
		display.syncExec(new Runnable() {
			public void run() {
				tableContentsOfLabel.setText("Contents of: "
						+ workerStateDir.getPath());
				table.removeAll();
				table.setData(TABLEDATA_DIR, workerStateDir);
			}
		});
		dirlist = getDirectoryList(workerStateDir);

		sortlist(dirlist);

		/* always include the parent directory */

		File par = workerStateDir.getParentFile();
		if (par != null) {
			FileInfo info;
			try {
				info = new FileInfo(par, iconcache);
				info.display = "..";
				workerAddFileDetails(info);
			} catch (IOException e) {
				// nothing...
			}
		}

		for (int i = 0; (!workerCancelled) && (i < dirlist.length); i++) {
			FileInfo info;
			try {
				info = new FileInfo(dirlist[i], iconcache);
				workerAddFileDetails(info);
			} catch (IOException e) {
				/* file not found; show red?? */
			}
		}

		/* now loop over table items, filling in their details. */
		while (!workerCancelled && workerDelayedInfo()) {
			/* nothing */
		}

	}

	private boolean workerDelayedInfo() {

		/* out of items? */
		if (workerInfoIdx >= workerInfos.size())
			return false;

		/* no, so do one ... */
		workerInfos.get(workerInfoIdx).fullDetails();

		final int wi = workerInfoIdx;

		/* tell the display to update it now */
		/*
		 * XXX this is screwy because it assumes the index in the table is the
		 * same as the one in our infos array. we should do getItembyData or
		 * whatever, if possible.
		 */
		display.syncExec(new Runnable() {
			public void run() {
				if (shell.isDisposed())
					return;
				/* XXX get idx from table */
				TableItem tableItem = table.getItem(wi);
				updatestrings(tableItem);
			}
		});
		/* next ... */
		workerInfoIdx++;
		return true;
	}

	/**
	 * Update the strings for the item at the specified index.
	 * 
	 * @param wi
	 *            the index
	 */
	private void updatestrings(TableItem tableItem) {
		FileInfo fi = (FileInfo) tableItem.getData(TABLEITEMDATA_INFO);
		FileData fd = fi.getData();

		final String[] strings = new String[] { fi.display, fi.getType(),
				fi.getSize(), fi.getDate(), fi.getHash(), };

		/*
		 * final Image[] images = new Image[] { fi.getIcon(),
		 * fd.seen?iconcache.stockImages[iconcache.iconOpenDrive]
		 * :iconcache.stockImages[iconcache.iconClosedDrive] };
		 */

		tableItem.setText(strings);
		/* can have images for each column! */
		// tableItem.setImage(images);
		//System.out.println("redrawing item");
		tableItem.setImage(new Image[] {
				fi.getIcon(),
				(fd != null && fd.seen)?iconcache.stockImages[iconcache.iconOpenEye]:
				iconcache.stockImages[iconcache.iconEmpty] });
	}

	/**
	 * Adds a file's detail information to the directory list, and into the
	 * to-do list.
	 */
	private void workerAddFileDetails(final FileInfo info) {
		workerInfos.add(info);

		/* only the name is available to start. */
		/*
		 * XXX revisit this; it seems that when we have a file object, we have a
		 * lot of stuff for free...
		 */
		final String[] strings = new String[] { info.display, "", "", "", "" };

		display.syncExec(new Runnable() {
			public void run() {
				// guard against the shell being closed before this runs
				if (shell.isDisposed())
					return;
				TableItem tableItem = new TableItem(table, 0);
				tableItem.setText(strings);
				tableItem.setImage(info.getIcon());
				tableItem.setData(TABLEITEMDATA_INFO, info);
			}
		});
	}

	/**
	 * Instances of this class manage a progress dialog for file operations.
	 */
	class ProgressDialog {
		public final static int COPY = 0;

		public final static int DELETE = 1;

		public final static int MOVE = 2;

		Shell shell;

		Label messageLabel, detailLabel;

		ProgressBar progressBar;

		Button cancelButton;

		boolean isCancelled = false;

		final String operationKeyName[] = { "Copy", "Delete", "Move" };

		/**
		 * Creates a progress dialog but does not open it immediately.
		 * 
		 * @param parent
		 *            the parent Shell
		 * @param style
		 *            one of COPY, MOVE
		 */
		public ProgressDialog(Shell parent, int style) {
			shell = new Shell(parent, SWT.BORDER | SWT.TITLE
					| SWT.APPLICATION_MODAL);
			GridLayout gridLayout = new GridLayout();
			shell.setLayout(gridLayout);
			shell.setText(operationKeyName[style]);
			shell.addShellListener(new ShellAdapter() {
				public void shellClosed(ShellEvent e) {
					isCancelled = true;
				}
			});

			messageLabel = new Label(shell, SWT.HORIZONTAL);
			messageLabel.setLayoutData(new GridData(GridData.FILL_HORIZONTAL
					| GridData.VERTICAL_ALIGN_FILL));
			messageLabel.setText(operationKeyName[style]);

			progressBar = new ProgressBar(shell, SWT.HORIZONTAL | SWT.WRAP);
			progressBar.setLayoutData(new GridData(GridData.FILL_HORIZONTAL
					| GridData.VERTICAL_ALIGN_FILL));
			progressBar.setMinimum(0);
			progressBar.setMaximum(0);

			detailLabel = new Label(shell, SWT.HORIZONTAL);
			GridData gridData = new GridData(GridData.FILL_HORIZONTAL
					| GridData.VERTICAL_ALIGN_BEGINNING);
			gridData.widthHint = 400;
			detailLabel.setLayoutData(gridData);

			cancelButton = new Button(shell, SWT.PUSH);
			cancelButton.setLayoutData(new GridData(
					GridData.HORIZONTAL_ALIGN_END
							| GridData.VERTICAL_ALIGN_FILL));
			cancelButton.setText("Cancel");
			cancelButton.addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent e) {
					isCancelled = true;
					cancelButton.setEnabled(false);
				}
			});
		}

		/**
		 * Sets the detail text to show the filename along with a string
		 * representing the operation being performed on that file.
		 * 
		 * @param file
		 *            the file to be detailed
		 * @param operation
		 *            one of COPY, DELETE
		 */
		public void setDetailFile(File file, int operation) {
			detailLabel.setText(operationKeyName[operation] + ": " + file);
		}

		/**
		 * Returns true if the Cancel button was been clicked.
		 * 
		 * @return true if the Cancel button was clicked.
		 */
		public boolean isCancelled() {
			return isCancelled;
		}

		/**
		 * Sets the total number of work units to be performed.
		 * 
		 * @param work
		 *            the total number of work units
		 */
		public void setTotalWorkUnits(int work) {
			progressBar.setMaximum(work);
		}

		/**
		 * Adds to the total number of work units to be performed.
		 * 
		 * @param work
		 *            the number of work units to add
		 */
		public void addWorkUnits(int work) {
			setTotalWorkUnits(progressBar.getMaximum() + work);
		}

		/**
		 * Sets the progress of completion of the total work units.
		 * 
		 * @param work
		 *            the total number of work units completed
		 */
		public void setProgress(int work) {
			progressBar.setSelection(work);
			while (display.readAndDispatch()) {
			} // enable event processing
		}

		/**
		 * Adds to the progress of completion of the total work units.
		 * 
		 * @param work
		 *            the number of work units completed to add
		 */
		public void addProgress(int work) {
			setProgress(progressBar.getSelection() + work);
		}

		/**
		 * Opens the dialog.
		 */
		public void open() {
			shell.pack();
			final Shell parentShell = (Shell) shell.getParent();
			Rectangle rect = parentShell.getBounds();
			Rectangle bounds = shell.getBounds();
			bounds.x = rect.x + (rect.width - bounds.width) / 2;
			bounds.y = rect.y + (rect.height - bounds.height) / 2;
			shell.setBounds(bounds);
			shell.open();
		}

		/**
		 * Closes the dialog and disposes its resources.
		 */
		public void close() {
			shell.close();
			shell.dispose();
			shell = null;
			messageLabel = null;
			detailLabel = null;
			progressBar = null;
			cancelButton = null;
		}
	}

	/**
	 * This interface is used for functions passed to applytoselected.
	 * 
	 * @see applytoselected
	 */
	private interface FileAction {
		abstract void act(FileInfo fi, FileData fd);
	}
}
