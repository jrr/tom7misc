/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is leech.
 *
 * The Initial Developer of the Original Code is Simon Windmill.
 * Portions created by the Initial Developer are Copyright (C) 2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *  Simon Windmill <siw@coolpowers.com>
 *  Andy Edmonds <aedmonds@mindspring.com>
 *  David Illsley <illsleydc@bigfoot.com>
 *  HJ van Rantwijk <bugs4HJ@netscape.net>
 *  Pavol Vaskovic <pali@pali.sk>
 *  Scott R. Turner <srt@aero.org>
 *  Tim Williamson <chsman@hotmail.com>
 *  Martin.T.Kutschker <Martin.T.Kutschker@blackbox.net>
 *  Tom Murphy <withheld> http://tom7.org/
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

// Global variables 
var defaultDir 		= new String;
var fileExtensions	= new String;
var showManager;
var overwriteHow;
var wgetLocation	= new String;
var tmpDir		= new String;
var makeDirs;
var useMozUA;
var usePrefix;

var filePrefix		= new String;
var leechedURLs		= new Array;
var numLeechedURLs;
var urlFile;

// Preferences observer object; implements nsIObserver
function llPrefObserver()
{
  try {
    var pbi = pref.QueryInterface(Components.interfaces.nsIPrefBranchInternal);
    pbi.addObserver(this.domain, this, false);
  } catch(ex) {
    dump("leech: Failed to observe prefs: " + ex + "\n");
  }
}
llPrefObserver.prototype =
{
  domain: "leech.",
  observe: function(subject, topic, prefName)
  {
    // when leech pref was changed, we reinitialize 
    try{
      leechInit();
    } catch(err){}
  }
}  


// this code is performed when the script is loaded:
// get the preferences service
var prefService = Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefService);
var pref = prefService.getBranch(null);
var llPref = prefService.getBranch("leech.");
window.leechPrefObserver = new llPrefObserver();


// do initialisation stuff, like read the preferences
function leechInit()
{ 
	// load preferences into JS variables
    // or restore default prefs if none are found
	dump("leech: init\n");
	window.removeEventListener("load", leechInit, true);

	// read prefs or set defaults if no prefs exist
	try
	{
		leechReadPref();  
	}
	catch(e)
	{
		dump("leech: Setting Default Preferences\n");
		llPref.setCharPref("defaultDir", "c:\\downloads\\");
        // tom added some defaults
		llPref.setCharPref("fileExtensions", 
           "jpg;jpeg;gif;png;mpg;mpeg;avi;mov;asx;wmv;zip;rar;gz");
		llPref.setBoolPref("showManager", true);
		llPref.setIntPref("overwriteHow", 1);
		llPref.setCharPref( "wgetLocation", "c:\\wget\\wget.exe" );
		llPref.setCharPref( "tmpDir", "c:\\temp\\" );
		llPref.setBoolPref( "makeDirs", false );
		llPref.setBoolPref( "useMozUA", true );
		llPref.setBoolPref( "usePrefix", true );
		leechReadPref();
	}  
}

function leechReadPref() {
  defaultDir 	= llPref.getCharPref("defaultDir");
  fileExtensions	= llPref.getCharPref("fileExtensions");
  showManager	= llPref.getBoolPref("showManager"); 
  overwriteHow	= llPref.getIntPref("overwriteHow");
  wgetLocation	= llPref.getCharPref( "wgetLocation" );
  tmpDir		= llPref.getCharPref( "tmpDir" );
  makeDirs	= llPref.getBoolPref( "makeDirs" );
  useMozUA	= llPref.getBoolPref( "useMozUA" );
  usePrefix	= llPref.getBoolPref( "usePrefix" );
}

// compares file extension from URL to list of valid extensions in prefs
// also does some security checks
function canLeechLink(url)
{
	var validext, urlext, theurl;
	
	theurl = url;
	
	// test for insecure links first
	if (theurl.substr(0, 11).toLowerCase() == 'javascript:')
		return false;
		
	if (theurl.substr(0, 7).toLowerCase() == 'mailto:')
		return false;
		
	// get the extension from the url
	dotpos = theurl.lastIndexOf('.');
	if (dotpos == -1)
		return false;
		
	urlext = theurl.substr(dotpos+1, theurl.length - dotpos).toLowerCase();
	if (urlext.length < 1)
		return false;
	
	
	// now get the array of valid extensions
	validexts = fileExtensions.split(';');
	
	
	if (validexts.length > 0)
	{
		for (ext = 0; ext < validexts.length; ext++)
		{
			if (urlext == validexts[ext].toLowerCase())
				return true;
		}
	}
	
	return false;
	
}



// this is called for each MIME header found, and handles the actual saving of the file
function myfoundHeaderInfo(aSniffer, aData)
{
	// get the local filename
	var defaultFilename = getDefaultFileName(aData.fileName, 
	                   aSniffer.suggestedFileName, 
	                   aSniffer.uri,
	                   null);
	
	// create the local file
	var lf = Components.classes["@mozilla.org/file/local;1"].createInstance(Components.interfaces.nsILocalFile);			  			  
	lf.initWithPath(defaultDir + filePrefix + defaultFilename);
	
	// Tom Murphy's new overwrite code
 	// check if the file exists. If it does, use the overwriteHow pref to
        // determine what to do.
 	if ( lf.exists () )
 	{
 
 		switch(overwriteHow)
 		{
	 		// overwrite
	 		case 0: break;
	 		// prompt
	 		case 1: if (!confirm(defaultDir + filePrefix + defaultFilename + ' exists, do you want to overwrite?')) return;
	 			break;
	 		// ignore
	 		case 2: return;
	 		// auto-rename
	 		case 3: 
	 		{
	 			var i = 1;
	 			do {
	 				i = i + 1;
	 				lf.initWithPath(defaultDir + filePrefix + i + "_" + defaultFilename);
	 			} while ( lf.exists () );
	 			break;
	 		}
 		}
 	}	
	  
	// setup download data
	var source = aSniffer.uri;
	  var persistArgs = {
	    source      : source,
	    contentType : aSniffer.contentEncodingType,
	    target      : lf,
	    postData    : null,
	    bypassCache : false
	  };

	// create a persistant browser
	var persist = makeWebBrowserPersist();
	const nsIWBP = Components.interfaces.nsIWebBrowserPersist;
	const flags = nsIWBP.PERSIST_FLAGS_NO_CONVERSION | nsIWBP.PERSIST_FLAGS_REPLACE_EXISTING_FILES;
	persist.persistFlags = flags | nsIWBP.PERSIST_FLAGS_BYPASS_CACHE;
	
	// add the file to the download manager    				
	var dlmgr = Components.classes['@mozilla.org/download-manager;1'].getService();
	dlmgr = dlmgr.QueryInterface(Components.interfaces.nsIDownloadManager);	
	dlmgr.addDownload(source, persistArgs.target, null, null, null, persist);
	
	// if we're supposed to show the manager, check if it's around, if not make it
	if (showManager)
	{
		var windowMediator = Components.classes['@mozilla.org/appshell/window-mediator;1'].getService();
		windowMediator = windowMediator.QueryInterface(nsIWindowMediator);
	
		var dlmgrWindow = windowMediator.getMostRecentWindow("Download:Manager");
		if (dlmgrWindow)
		{
			dlmgrWindow.focus();
		}
		/*else
		{
			dlmgr.open(getBrowser().contentWindow);
		}*/
		// TODO: find out why it creates a new window for each new download,
		// then we can bring back the open function
	}
	
	
	// and start the download
    // tom7: added nulls as per Markus's suggestion:
	persist.saveURI(source, null, null, persistArgs.postData, null, persistArgs.target);
	
    // (does this now allow setting of referer?)

	// okay, so I have a thought about making this work for
	// referer-protected sites, but it will need a modification to
	// Mozilla. In SaveURIInternal, we need to add the referer to the
	// httpchannel - there should be an example in LoadURIInteral (or
	// whatever)
}


// main function, iterate through each link in the document
function doLeech(doc, forceSeparate, useWget)
{			
	urls = doc.links;
	
	baseurl = doc.location;
	

	var written;
	if ( useWget )
	{
		// first we have to dump all the links to file
		urlFile = Components.classes["@mozilla.org/filespec;1"].createInstance(Components.interfaces.nsIFileSpec);
	
		urlFile.nativePath = tmpDir + "leechurls.txt";
	
		urlFile.openStreamForWriting();
	}
		
	// create a referer URI from current page
	var ioService = Components.classes["@mozilla.org/network/io-service;1"].getService(Components.interfaces.nsIIOService);
	var refnsiURI  = ioService.newURI(baseurl, null, null);
		
	var i;	
	for (i = 0; i < urls.length; i++)
	{
		var url = urls[i].href;
		
		// check for duplicates
		dupe = false;
		var j;
		for (j = 0; j < numLeechedURLs; j++)
			if (url == leechedURLs[j])
				dupe = true;
				
				
		leechedURLs[numLeechedURLs] = url;
		numLeechedURLs++;
		
		
		if ( canLeechLink(url) && (dupe == false) )
		{					
			if (forceSeparate)
			{
				// open a new tab for each URL, using the current page
				// as the referer
				var tab = getBrowser().addTab(url, refnsiURI);
								
				saveURL(url, null, "SaveImageTitle", false);
				
				//getBrowser().selectedTab = tab;
				// I tried before to close all the tabs automatically.. it failed (they closed too quickly)
				

			}
			else if ( useWget )
			{				
				urlFile.write( url, url.length, written ); urlFile.endLine();
			}
			else
			{
				// setup a sniffer callback for checking each file.. 
				var data = {
				    url: url,
				    fileName: null,
				    filePickerTitle: null,
				    document: null,
				    bypassCache: false
				  };
				  var sniffer = new nsHeaderSniffer(url, myfoundHeaderInfo, data);
			}
		}
	}
	
	// close the url file if open	
	if ( useWget )
	{
		urlFile.closeStream();	
		
		
		// create a file object for the wget executable
		var theapp = Components.classes["@mozilla.org/filespec;1"].createInstance(Components.interfaces.nsIFileSpec);			  			  
	
		theapp.nativePath  = wgetLocation;
	
		makedirargs = "-nd ";
		if ( makeDirs )
		{
			makedirargs = "-x ";
		}
		
		uaargs = "";
		if ( useMozUA )
		{
			uaargs = "-U \"" + navigator.userAgent + "\" ";
		}
		
		args = uaargs + makedirargs + "-P " + defaultDir + filePrefix + " --referer=" + baseurl + " -i " + urlFile.nativePath;
		
		theapp.execute( args );
	
		// finally, delete the urls file
		//urlFile.delete( true );
	}


}



function startLeech(forceSeparate, askPrefix, useWget)
{
	filePrefix = "";
	numLeechedURLs = 0;
	
	if ( askPrefix )
	{
		filePrefix = prompt('Enter prefix for files', '');
	}
	
	var contentFrames = _content.frames;
	numFrames = contentFrames.length;
	if ( numFrames > 0 )
	{
		var i;
		for (i = 0; i < numFrames; i++)
		{
			doLeech( contentFrames[i].document, forceSeparate, useWget );
		}
	}	
	else
	{
		doLeech( getBrowser().contentDocument, forceSeparate, useWget );		
	}
}

///////////////////////////////////////////////////////////////////////////////

// these functions are called from the menu items
function cmdQuickLeech()
{
	startLeech( false, false, false );
}

function cmdLeechSeparately()
{
	startLeech( true, false, false );
}

function cmdLeechWithPrefix()
{
	if ( usePrefix )	
		startLeech( false, true, true );
	else
		startLeech( false, true, false );
}

function cmdLeechWithWget()
{
	startLeech (false, false, true );
	
	
	
}

function cmdLeechEntireSite()
{
	baseurl = getBrowser().contentDocument.location;
	
	// create a file object for the wget executable
	var theapp = Components.classes["@mozilla.org/filespec;1"].createInstance(Components.interfaces.nsIFileSpec);			  			  

	theapp.nativePath  = wgetLocation;

	makedirargs = "-x ";
	
	uaargs = "";
	if ( useMozUA )
	{
		uaargs = "-U \"" + navigator.userAgent + "\" ";
	}
	
	recurseargs = "-r -np ";
	
	args = uaargs + makedirargs + "-P " + defaultDir + " " + recurseargs + baseurl;
	
	theapp.execute( args );
	
}

