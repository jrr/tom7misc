
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
 * The Original Code is the Linky
 *
 * The Initial Developer of the Original Code is Henrik Gemal.
 * Portions created by the Initial Developer are Copyright (C) 2002-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Henrik Gemal <linky@gemal.dk> http://gemal.dk
 *   Tom Murphy <withheld> http://tom7.org/
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

var checkboxes  = ["showManager", "useMozUA", "usePrefix", "makeDirs" ];
var radiogroups = ["overwriteHow"];
var textboxes   = ["defaultDir", "fileExtensions", "wgetLocation", "tmpDir"];

// This gets called automatically from Mozilla Application Suite
function Startup() {
  // set default prefs
  leechPrefSet();

}


// This has to be called manually from Mozilla Firebird
// (in pref-panel-extension onload)
function leechPrefInit() {

  leechInit();

  /* set the html items */
  /* first the checkboxes. If it has a prefstring, then get the
     corresponding pref and use its value to set the box's value.
  */
  for (var i = 0; i < checkboxes.length; ++i) {
    var checkbox = document.getElementById(checkboxes[i]);
    try {
      checkbox.checked = pref.getBoolPref(checkbox.getAttribute("prefstring"));
    } catch(err) {
      checkbox.checked = true;
    }
  }

  /* menus work like radiogroups (?? from linky) */
  for (i = 0; i < radiogroups.length; ++i) {
    var radiogroup = document.getElementById(radiogroups[i]);
    try {
      radiogroup.selectedItem = 
        radiogroup.getElementsByAttribute
        ("value", pref.getIntPref(radiogroup.getAttribute("prefstring")))[0];
    } catch(err) {
     // radiogroup.checked = true;
    }
  }

  /* text boxes */
  for (i = 0; i < textboxes.length; ++i) {
    var textbox = document.getElementById(textboxes[i]);
    try {
      textbox.value = 
        pref.getCharPref(textbox.getAttribute("prefstring"));
    } catch(err) {
      textbox.value = "";
    }
  }


  // Manually call the Startup since Mozilla Firebird doesn't call it
  Startup();
}

// Manually save prefs since Mozilla Firebird doesn't do it
// called on dialog ok in pref-panel-extension
function leechPrefSave() {
  var i;
  for (i = 0; i < checkboxes.length; ++i) {
    var checkbox = document.getElementById(checkboxes[i]);
    try {
      pref.setBoolPref(checkbox.getAttribute("prefstring"), checkbox.checked);
    } catch(err) { }
  }
  for (i = 0; i < radiogroups.length; ++i) {
    var radiogroup = document.getElementById(radiogroups[i]);
    try {
      pref.setIntPref(radiogroup.getAttribute("prefstring"), radiogroup.selectedItem.value);
    } catch(err) { }
  }
  for (i = 0; i < textboxes.length; ++i) {
    var textbox = document.getElementById(textboxes[i]);
    try {
      pref.setCharPref(textbox.getAttribute("prefstring"), textbox.value);
    } catch(err) { }
  }
}

