;; Copyright (c) 2010 Phil Hargett

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(in-tag-library :i18n)

(+tag-library :html)
(+tag-library :script)
(+tag-library :standard)

(defhtmltag standard-locale-service 
    :script-libraries `("/meta/clients/locales.js")
    :content (list 
	      (var 'locale-package *locale-package*)
	      (var 'locale-service-url "/meta/services/locales")))

(defhtmltag standard-pane
    :style "
.pane {
  position : absolute;
  z-index : 100;
  -webkit-border-radius : 10px;
  -moz-border-radius : 10px;
  background-color : rgba(0,0,0,0.5);
}

.pane hidden {
  display : none;
}

.pane_content {
  display : block;
}

"
    :script "
function showPane(pane){
  $(pane).css('left',$('body').width() / 2 - $(pane).width()/2);
  $(pane).css('top',$('body').height() / 2 - $(pane).height()/2);
  $(pane).removeClass('hidden');
}

function hidePane(pane){
  $(pane).addClass('hidden');
}

"
    :content (with-slots (id class _body) *current-tag* 
	       (div (+@ :class (concatenate 'string "pane hidden" (or class "")) :id id) 		    
		    (div (+@ :class "pane_content") _body))))

(defhtmltag standard-dropdown-menu 
    :style "

.hidden {
  display : none;
}

.dropdown {
  margin : 1em;
  text-align : left;
}

.dropdown .label {
  display : inline;
}

.dropdown .choices_container {
  display : inline;
  float :right;
  width : 100px;
}

.dropdown .value {
  display : inline-block;
  border : 1px solid #silver;
  cursor : pointer;
  min-width : 100px;
  text-align : right;
}

.dropdown .choices {
  position : absolute;
  border : 1px solid white;
  cursor : pointer;
  width : 100px;
  background-color : rgba(0,0,0,0.5);
}

.dropdown .choice {
  text-align : right;
}

.dropdown .choice:hover {
  background-color : rgba(80%,80%,80%,0.7);
}

"
    :ready "
$('.dropdown .value').click(function(e){
  $('.choices').toggleClass('hidden');
});

$('.dropdown .choice').click(function(e){
  var dropdown = this.parentElement.parentElement.parentElement;
  if($(this).hasClass('add')) {
    // trigger event to add a choice
    $('.choices',dropdown).toggleClass('hidden');
    $(dropdown).trigger('addChoice');
  } else {
    // trigger event to select a choice
    $('.value',dropdown).text($(this).text());
    $(dropdown).trigger('selectChoice');
    $('.choices',dropdown).toggleClass('hidden');
  }
});
"
    :attributes ((label :initarg :label)
		 (default-value :initarg :default)
		 (choices :initarg :choices)
		 (add :initform nil :initarg :add))

    :content (with-slots (label default-value choices add id) *current-tag*
	       (let ((dropdown-id (or id (symbol-name (gensym)))))
		 (div (+@ :class "dropdown" :id dropdown-id)
		      (div (+@ :class "label") label)
		      (div (+@ :class "choices_container") 
			   (div (+@ :class "value") default-value)
			   (div (+@ :class "choices hidden")
				(append
				 (loop for choice in choices
				    collect (div (+@ :class "choice") choice))
				 (list (when add (div (+@ :class "choice add") 
						      (span (+@ :style "float : left;font-weight : bold") "+") 
						      (format nil (_  "Add ~a") add)))))))))))

(defhtmltag localizable-string-editor
    :style "
.string_editor td {
  padding : 0.5em;
  cellpadding : 0.5em;
}


.string_editor .label {
  vertical-align : top;
}

.string_editor .field {
  width : 100%;
  min-width : 300px;
}

"
    :script "
function showStringEditor(row) {
  stringName = $.trim($('.string_row .string_name').eq(row).text());
  $('#string_localization').val($.trim($('.string_row .string_localization').eq(row).text()));
  $('#string_description').val($.trim($('.string_row .string_description').eq(row).text()));
  $('#string_notes').val($.trim($('.string_row .string_notes').eq(row).text()));
  showPane('#string_editor');
}

function hideStringEditor() {
  $('#string_localization').val('');
  $('#string_notes').val('');
  hidePane('#string_editor');
}

function doSaveStringLocalization() {
  var localization = $.trim($('#string_localization').val());
  var notes = $.trim($('#string_notes').val());
  var description = $.trim($('#string_description').val());
  saveStringLocalization(localeServiceUrl,localePackage,localeName,stringName,localization,description,notes,function(){
    hideStringEditor();
    window.location=localeName;
  });
}

"
    :content (with-slots (class id) *current-tag*
	       (standard-pane (+@ :id (or id "string_editor") :class (concatenate 'string " string_editor " (or class "")))
			      (form (+@ :class "string_editor_form")	
				    (var 'string-name "")
				    (table
				     (tr (td (+@ :class "label") (_ "Localization")) 
					 (td (textarea (+@ :id "string_localization" :class "field" :rows 3 :cols 20))))
				     (tr (td (+@ :class "label") (_ "Notes")) 
					 (td (textarea (+@ :id "string_notes" :class "field" :rows 3 :cols 20))))
				     (tr (td (+@ :class "label") (_ "Description")) 
					 (td (textarea (+@ :id "string_description" :class "field" :rows 3 :cols 20))))
				     (tr (td (+@ :class "label") +nbsp+) 
					 (td (+@ :style "text-align:right") 
					     (standard-action-button (+@ :target "javascript:hideStringEditor()")  "Cancel")
					     (standard-action-button (+@ :target "javascript:doSaveStringLocalization()")  "OK"))))))))

(defhtmltag add-locale-pane 
    :style "
#add_locale td {
  padding : 0.5em;
  cellpadding : 0.5em;
}
"
    :script "
function showAddLocalePane() {
  showPane('#add_locale');
}

function hideAddLocalePane() {
  hidePane('#add_locale');
}

function doAddLocale() {
  var localeName=$('#locale_name').val();
  createLocale(localeServiceUrl,localePackage,localeName,function(){
   hideAddLocalePane();
   window.location=localeName;
  });
}

"
    :content (standard-pane (+@ :id "add_locale")
			    (table
			     (tr (td "Locale") (td (text-field (+@ :id "locale_name") )))
			     (tr (td +nbsp+) 
				 (td (+@ :style "text-align:right") 
				     (standard-action-button (+@ :target "javascript:hideAddLocalePane()") "Cancel")
				     (standard-action-button (+@ :target "javascript:doAddLocale()") "OK"))))))

(defhtmltag available-locales-list
    :attributes ( (default :initform "en_US" :initarg :default))
    :style "
.locale_chooser {
  width : 250px;
}

.locale_chooser h2 {
}

.locale_chooser ul.locale_list {
  width : 200px;
  display : none;
  text-align : right;

  list-style: none;  
  padding: 0 20px;  
  margin: 0;  
  font-size: 1.2em;  
}

.locale_chooser ul.locale_list li {
  border : 1px solid silver;
  padding-left : 0.5em;
  padding-right : 0.5em;
  position : relative;
}

.locale_chooser .arrow {
  background-color : #EEE;
  width : 40px;
}

.locale_chooser a {
  text-decoration : none;
}

.local_chooser .choice {
  display : inline;
  width : 100px;
  border : 1px solid silver;
  padding-left : 0.5em;
  padding-right : 0.5em;

  cursor : pointer;
}

"
    :ready "
$('#locales').bind('addChoice',function(e) {
  showAddLocalePane();
});
"
    :content (with-slots (default) *current-tag* 
	       (div (+@ :class "locale_chooser") 
		    ( standard-locale-service)
		    (standard-dropdown-menu 
		     (+@ :id "locales"
			 :label "Locale" 
			 :default default 
			 :choices (loop for loc in (append (get-available-locales))
				       collect (a (+@ :href loc) loc)) 
			 :add (_ "locale"))))))

(defhtmltag localizable-strings-list
    :attributes ( (locale :initform "en_US" :initarg :locale))
    :style "
.string_list table {
  border-spacing : 0px;
  border-collapse : collapse;
}

.string_list thead {
  font-weight : bold;
  text-align : center;
}

.string_list .search {
  width : 200px;
  padding : 0.1em;
  margin-left : 1.0em;
  margin-right : 1.0em;
  margin-top : 0.5em;
  margin-bottom : 0.5em;
  border : 1px solid silver;
  -webkit-border-radius : 10px;
  -moz-border-radius : 10px;
}

.string_list td {
  border : 1px solid silver;
  min-width : 200px;
}

"
    :content (with-slots (locale) *current-tag*
	       (div (+@ :class "string_list") 
		      (table  
		       (th (tr (td (h3 "String")) (td (h3 (_ "Localization"))) (td (h3 (_ "Description"))) (td (h3 (_ "Notes"))) )
			   (tr (td (text-field (+@ :class "search"))) (td (+@ :colspan "4" :style "background-color : rgba(0,0,0,0.2)") +nbsp+)))
		       (loop for s in (get-localizable-strings)
			  for row = 0 then (1+ row)
			  collect  (tr (+@ :class "string_row")
				       (td (p (a (+@ :class "string_name" :href (format nil "javascript:showStringEditor(~s)" row)) s))
					   (ul (loop for context in (get-localization-contexts s)
						  collect (li context))))
				       (td (+@ :class "string_localization") (get-string-localization s locale)) 
				       (td (+@ :class "string_description") (get-localizable-string-notes s))
				       (td (+@ :class "string_notes")(get-string-localization-notes s locale))))))))


(defhtmltag localization-editor
    :attributes ( (locale :initform "en_US" :initarg :locale))
    :style "

.localization_editor {
  width : 800px;
}

.localization_editor .heading {
  width : 800px;
  border : 0px;
}

.localization_editor .heading .title {
  width : 600px;
}

.localization_editor .chooser {
  width : 200px;
}

.localization_editor a {
color: #CCE0ED; 
// text-decoration: none; 
}

.localization_editor a:hover { 
color: #D48361; 
}


"
    :content (with-slots (locale) *current-tag* 
	       (list
		(var 'locale-name locale)
		(div (+@ :class "localization_editor")
		     (table (+@ :class "heading") 
			    (tr (td (+@ :class "title") (_ "Localizable strings")) 
				(td (+@ :class "chooser") (available-locales-list (+@ :default locale)))))
		     (localizable-string-editor)
		     (add-locale-pane)
		     (localizable-strings-list (+@ :locale locale))))))
