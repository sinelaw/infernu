//#include "Node.idl"

// %{ C++
// #include "jspubtd.h"

// // windows.h #defines CreateEvent
// #ifdef CreateEvent
// #undef CreateEvent
// #endif
// %}

interface NodeIterator;
interface NodeFilter;
interface TreeWalker;
interface Location;

/**
 * The Document interface represents the entire HTML or XML document.
 * Conceptually, it is the root of the document tree, and provides the
 * primary access to the document's data.
 * Since elements, text nodes, comments, processing instructions, etc.
 * cannot exist outside the context of a Document, the Document
 * interface also contains the factory methods needed to create these
 * objects.
 *
 * For more information on this interface please see
 * http://dvcs.w3.org/hg/domcore/raw-file/tip/Overview.html
 */

[uuid(35dc5030-dc83-4291-88a2-0906c549788e)]
interface Document : Node {
  readonly attribute DocumentType         doctype;
  readonly attribute DOMImplementation    implementation;
  readonly attribute Element              documentElement;
  //  Element                 createElement([Null(Stringify)] in DOMString tagName);
  DocumentFragment        createDocumentFragment();
  // Text                    createTextNode(in DOMString data);
  // Comment                 createComment(in DOMString data);
  // CDATASection            createCDATASection(in DOMString data);
  // ProcessingInstruction   createProcessingInstruction(in DOMString target,
  //                                                           in DOMString data);
  // Attr                    createAttribute(in DOMString name);
  // NodeList                getElementsByTagName(in DOMString tagname);

  // // Introduced in DOM Level 2:
  // [optional_argc] Node    importNode(in Node importedNode,
  //                                          [optional] in boolean deep);
  // // Introduced in DOM Level 2:
  // Element                 createElementNS(in DOMString namespaceURI,
  //                                               [Null(Stringify)] in DOMString qualifiedName);
  // // Introduced in DOM Level 2:
  // Attr                    createAttributeNS(in DOMString namespaceURI,
  //                                                 in DOMString qualifiedName);
  // // Introduced in DOM Level 2:
  // NodeList                getElementsByTagNameNS(in DOMString namespaceURI,
  //                                                      in DOMString localName);
  // // Introduced in DOM Level 2:
  // Element                 getElementById(in DOMString elementId);
  // // Introduced in DOM Level 3:
  // readonly attribute DOMString       inputEncoding;
  // // Introduced in DOM Level 3:
  // readonly attribute DOMString       documentURI;
  // // Alias introduced for all documents in recent DOM standards
  // readonly attribute DOMString       URL;
  // // Introduced in DOM Level 3:
  // Node         adoptNode(in Node source);

  // /**
  //  * Create a range
  //  *
  //  * @see http://html5.org/specs/dom-range.html#dom-document-createrange
  //  */
  // Range              createRange();

  // [optional_argc] NodeIterator createNodeIterator(in Node root,
  //                                                       [optional] in unsigned long whatToShow,
  //                                                       [optional] in NodeFilter filter);
  // [optional_argc] TreeWalker   createTreeWalker(in Node root,
  //                                                     [optional] in unsigned long whatToShow,
  //                                                     [optional] in NodeFilter filter);

  // Event               createEvent(in DOMString eventType);


  // // HTML
  // /**
  //  * The window associated with this document.
  //  *
  //  * @see <http://www.whatwg.org/html/#dom-document-defaultview>
  //  */
  // readonly attribute Window   defaultView;

  // /**
  //  * @see <http://www.whatwg.org/html/#dom-document-characterset>
  //  */
  // readonly attribute DOMString      characterSet;
  // /**
  //  * @see <http://www.whatwg.org/html/#dom-document-dir>
  //  */
  //          attribute DOMString      dir;

  // /**
  //  * @see <http://www.whatwg.org/html/#dom-document-location>
  //  */
  // readonly attribute Location location;

  // /**
  //  * @see <http://www.whatwg.org/html/#document.title>
  //  */
  //          attribute DOMString      title;

  // /**
  //  * @see <http://www.whatwg.org/html/#dom-document-readystate>
  //  */
  // readonly attribute DOMString      readyState;
  // /**
  //  * @see <http://www.whatwg.org/html/#dom-document-lastmodified>
  //  */
  // readonly attribute DOMString      lastModified;
  // /**
  //  * @see <http://www.whatwg.org/html/#dom-document-referrer>
  //  */
  // readonly attribute DOMString      referrer;

  // /**
  //  * @see <http://www.whatwg.org/html/#dom-document-hasfocus>
  //  */
  // boolean      hasFocus();

  // /**
  //  * @see <http://www.whatwg.org/html/#dom-document-activeelement>
  //  */
  // readonly attribute Element  activeElement;

  // /**
  //  * Retrieve elements matching all classes listed in a
  //  * space-separated string.
  //  *
  //  * @see <http://www.whatwg.org/html/#dom-document-getelementsbyclassname>
  //  */
  // NodeList getElementsByClassName(in DOMString classes);


  // // CSSOM
  // /**
  //  * @see <http://dev.w3.org/csswg/cssom/#dom-document-stylesheets>
  //  */
  // readonly attribute StyleSheetList   styleSheets;

  // /**
  //  * This attribute must return the preferred style sheet set as set by the
  //  * author. It is determined from the order of style sheet declarations and
  //  * the Default-Style HTTP headers, as eventually defined elsewhere in the Web
  //  * Apps 1.0 specification. If there is no preferred style sheet set, this
  //  * attribute must return the empty string. The case of this attribute must
  //  * exactly match the case given by the author where the preferred style sheet
  //  * is specified or implied. This attribute must never return null.
  //  *
  //  * @see <http://dev.w3.org/csswg/cssom/#dom-document-preferredStyleSheetSet>
  //  */
  // readonly attribute DOMString preferredStyleSheetSet;

  // /**
  //  * This attribute indicates which style sheet set is in use. This attribute
  //  * is live; changing the disabled attribute on style sheets directly will
  //  * change the value of this attribute.
  //  *
  //  * If all the sheets that are enabled and have a title have the same title
  //  * (by case-sensitive comparisons) then the value of this attribute must be
  //  * exactly equal to the title of the first enabled style sheet with a title
  //  * in the styleSheets list. Otherwise, if style sheets from different sets
  //  * are enabled, then the return value must be null (there is no way to
  //  * determine what the currently selected style sheet set is in those
  //  * conditions). Otherwise, either all style sheets that have a title are
  //  * disabled, or there are no alternate style sheets, and
  //  * selectedStyleSheetSet must return the empty string.
  //  *
  //  * Setting this attribute to the null value must have no effect.
  //  *
  //  * Setting this attribute to a non-null value must call
  //  * enableStyleSheetsForSet() with that value as the function's argument, and
  //  * set lastStyleSheetSet to that value.
  //  *
  //  * From the DOM's perspective, all views have the same
  //  * selectedStyleSheetSet. If a UA supports multiple views with different
  //  * selected alternate style sheets, then this attribute (and the StyleSheet
  //  * interface's disabled attribute) must return and set the value for the
  //  * default view.
  //  *
  //  * @see <http://dev.w3.org/csswg/cssom/#dom-document-selectedStyleSheetSet>
  //  */
  // [binaryname(MozSelectedStyleSheetSet)]
  // attribute DOMString selectedStyleSheetSet;

  // /*
  //  * This property must initially have the value null. Its value changes when
  //  * the selectedStyleSheetSet attribute is set.
  //  *
  //  * @see <http://dev.w3.org/csswg/cssom/#dom-document-lastStyleSheetSet>
  //  */
  // readonly attribute DOMString lastStyleSheetSet;

  // /**
  //  * This must return the live list of the currently available style sheet
  //  * sets. This list is constructed by enumerating all the style sheets for
  //  * this document available to the implementation, in the order they are
  //  * listed in the styleSheets attribute, adding the title of each style sheet
  //  * with a title to the list, avoiding duplicates by dropping titles that
  //  * match (case-sensitively) titles that have already been added to the
  //  * list.
  //  *
  //  * @see <http://dev.w3.org/csswg/cssom/#dom-document-styleSheetSets>
  //  */
  // readonly attribute nsISupports styleSheetSets;

  // /**
  //  * Calling this method must change the disabled attribute on each StyleSheet
  //  * object with a title attribute with a length greater than 0 in the
  //  * styleSheets attribute, so that all those whose title matches the name
  //  * argument are enabled, and all others are disabled. Title matches must be
  //  * case-sensitive. Calling this method with the empty string disables all
  //  * alternate and preferred style sheets (but does not change the state of
  //  * persistent style sheets, that is those with no title attribute).
  //  *
  //  * Calling this method with a null value must have no effect.
  //  *
  //  * Style sheets that do not have a title are never affected by this
  //  * method. This method does not change the values of the lastStyleSheetSet or
  //  * preferredStyleSheetSet attributes.
  //  *
  //  * @see <http://dev.w3.org/csswg/cssom/#dom-document-enableStyleSheetsForSet>
  //  */
  // [binaryname(MozEnableStyleSheetsForSet)]
  // void enableStyleSheetsForSet(in DOMString name);


  // // CSSOM-View
  // /**
  //  * Returns the element from the caller's document at the given point,
  //  * relative to the upper-left-most point in the (possibly scrolled)
  //  * window or frame.
  //  *
  //  * If the element at the given point belongs to another document (such as
  //  * an iframe's subdocument), the element in the calling document's DOM
  //  * (e.g. the iframe) is returned. If the element at the given point is
  //  * anonymous or XBL generated content, such as a textbox's scrollbars, then
  //  * the first non-anonymous parent element (that is, the textbox) is returned.
  //  *
  //  * This method returns null if either coordinate is negative, or if the
  //  * specified point lies outside the visible bounds of the document.
  //  *
  //  * Callers from XUL documents should wait until the onload event has fired
  //  * before calling this method.
  //  *
  //  * @see <http://dev.w3.org/csswg/cssom-view/#dom-document-elementfrompoint>
  //  */
  // Element             elementFromPoint(in float x, in float y);


  // // Mozilla extensions
  // /**
  //  * @see <https://developer.mozilla.org/en/DOM/document.contentType>
  //  */
  // readonly attribute DOMString      contentType;

  // /**
  //  * True if this document is synthetic : stand alone image, video, audio file,
  //  * etc.
  //  */
  // readonly attribute boolean        mozSyntheticDocument;

  // /**
  //  * Returns the script element whose script is currently being processed.
  //  *
  //  * @see <https://developer.mozilla.org/en/DOM/document.currentScript>
  //  */
  // readonly attribute Element  currentScript;

  // /**
  //  * Release the current mouse capture if it is on an element within this
  //  * document.
  //  *
  //  * @see <https://developer.mozilla.org/en/DOM/document.releaseCapture>
  //  */
  // void releaseCapture();

  // /**
  //  * Use the given DOM element as the source image of target |-moz-element()|.
  //  *
  //  * This function introduces a new special ID (called "image element ID"),
  //  * which is only used by |-moz-element()|, and associates it with the given
  //  * DOM element.  Image elements ID's have the higher precedence than general
  //  * HTML id's, so if |document.mozSetImageElement(<id>, <element>)| is called,
  //  * |-moz-element(#<id>)| uses |<element>| as the source image even if there
  //  * is another element with id attribute = |<id>|.  To unregister an image
  //  * element ID |<id>|, call |document.mozSetImageElement(<id>, null)|.
  //  *
  //  * Example:
  //  * <script>
  //  *   canvas = document.createElement("canvas");
  //  *   canvas.setAttribute("width", 100);
  //  *   canvas.setAttribute("height", 100);
  //  *   // draw to canvas
  //  *   document.mozSetImageElement("canvasbg", canvas);
  //  * </script>
  //  * <div style="background-image: -moz-element(#canvasbg);"></div>
  //  *
  //  * @param aImageElementId an image element ID to associate with
  //  * |aImageElement|
  //  * @param aImageElement a DOM element to be used as the source image of
  //  * |-moz-element(#aImageElementId)|. If this is null, the function will
  //  * unregister the image element ID |aImageElementId|.
  //  *
  //  * @see <https://developer.mozilla.org/en/DOM/document.mozSetImageElement>
  //  */
  // void mozSetImageElement(in DOMString aImageElementId,
  //                         in Element aImageElement);

  // /**
  //  * Element which is currently the full-screen element as per the DOM
  //  * full-screen api.
  //  *
  //  * @see <https://wiki.mozilla.org/index.php?title=Gecko:FullScreenAPI>
  //  */
  // readonly attribute Element mozFullScreenElement;

  // /**
  //  * Causes the document to leave DOM full-screen mode, if it's in
  //  * full-screen mode, as per the DOM full-screen api.
  //  *
  //  * @see <https://wiki.mozilla.org/index.php?title=Gecko:FullScreenAPI>
  //  */
  // void mozCancelFullScreen();

  // /**
  //  * Denotes whether this document is in DOM full-screen mode, as per the DOM
  //  * full-screen api.
  //  *
  //  * @see <https://wiki.mozilla.org/index.php?title=Gecko:FullScreenAPI>
  //  */
  // readonly attribute boolean mozFullScreen;

  // /**
  //  * Denotes whether the full-screen-api.enabled is true, no windowed
  //  * plugins are present, and all ancestor documents have the
  //  * allowfullscreen attribute set.
  //  *
  //  * @see <https://wiki.mozilla.org/index.php?title=Gecko:FullScreenAPI>
  //  */
  // readonly attribute boolean mozFullScreenEnabled;

  // /**
  //  * The element to which the mouse pointer is locked, if any, as per the
  //  * DOM pointer lock api.
  //  *
  //  * @see <http://dvcs.w3.org/hg/pointerlock/raw-file/default/index.html>
  //  */
  // readonly attribute Element mozPointerLockElement;

  // /**
  //  * Retrieve the location of the caret position (DOM node and character
  //  * offset within that node), given a point.
  //  *
  //  * @param x Horizontal point at which to determine the caret position, in
  //  *          page coordinates.
  //  * @param y Vertical point at which to determine the caret position, in
  //  *          page coordinates.
  //  */
  // nsISupports /* CaretPosition */ caretPositionFromPoint(in float x, in float y);

  // /**
  //  * Exit pointer is lock if locked, as per the DOM pointer lock api.
  //  *
  //  * @see <http://dvcs.w3.org/hg/pointerlock/raw-file/default/index.html>
  //  */
  // void mozExitPointerLock();

  // /**
  //  * Visibility API implementation.
  //  */
  // readonly attribute boolean hidden;
  // readonly attribute boolean mozHidden;
  // readonly attribute DOMString visibilityState;
  // readonly attribute DOMString mozVisibilityState;

  // /**
  //  * Returns "BackCompat" if we're in quirks mode or "CSS1Compat" if we're in
  //  * strict mode.  (XML documents are always in strict mode.)
  //  */
  // readonly attribute DOMString compatMode;

  // /**
  //  * Return nodes that match a given CSS selector.
  //  *
  //  * @see <http://dev.w3.org/2006/webapi/selectors-api/>
  //  */
  // Element querySelector([Null(Stringify)] in DOMString selectors);
  // NodeList querySelectorAll([Null(Stringify)] in DOMString selectors);
};
