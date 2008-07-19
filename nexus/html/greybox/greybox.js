/****
 Last Modified: 29/06/06 13:18:20

 GreyBox - Smart pop-up window
   Copyright Amir Salihefendic 2006
 AUTHOR
   4mir Salihefendic (http://amix.dk) - amix@amix.dk
 VERSION
	 3.25
 LICENSE
  GPL (read more in GPL.txt)
 SITE
  http://orangoo.com/labs/GreyBox/
****/
var GB_CURRENT = null;
var GB_ONLY_ONE = null;

function GreyBox() {
  //Use mutator functions (since the internal stuff may change in the future)
  this.type = "page";
  this.overlay_click_close = true;
  this.img_dir = "greybox/";
  this.overlay_color = "dark";
  this.image_click_close = false; //Click on a picture in GB_showImage results in GB_hide

  this.center_window = true;

  this.g_window = null;
  this.g_container = null;
  this.iframe = null;
  this.overlay = null;
  this.timeout = null;

  this.defaultSize();


  this.url = "";
  this.caption = "";
}

////
// Configuration functions (the functions you can call)
////
/**
  Set the width and height of the GreyBox window.
  Images and notifications are auto-set.
  **/
GreyBox.prototype.setDimension = function(width, height) {
  this.height = height;
  this.width = width;
}

GreyBox.prototype.setFullScreen = function(bool) {
  this.full_screen = bool;
}

/**
  Type can be: page, image
  **/
GreyBox.prototype.setType = function(type) {
  this.type = type;
}

/**
  If bool is true the window will be centered vertically also
  **/
GreyBox.prototype.setCenterWindow = function(bool) {
  this.center_window = bool;
}

/**
  Set the path where images can be found.
  Can be relative: greybox/
  Or absolute: http://yoursite.com/greybox/
  **/
GreyBox.prototype.setImageDir = function(dir) {
  this.img_dir = dir;
}

/**
  If bool is true the grey overlay click will close greybox.
  **/
GreyBox.prototype.setOverlayCloseClick = function(bool) {
  this.overlay_click_close = bool;
}

/**
  Overlay can either be "light" or "dark".
  **/
GreyBox.prototype.setOverlayColor = function(color) {
  this.overlay_color = color;
}

/**
  Set a function that will be called when GreyBox closes
  **/
GreyBox.prototype.setCallback = function(fn) {
  this.callback_fn = fn;
}


////
// Show hide functions
////
/**
  Show the GreyBox with a caption and an url
  **/
GreyBox.prototype.show = function(caption, url) {
  GB_CURRENT = this;

  this.url = url;
  this.caption = caption;

  //Be sure that the old loader and dummy_holder are removed
  AJS.map(AJS.$bytc("div", "GB_dummy"), function(elm) { AJS.removeElement(elm) });
  AJS.map(AJS.$bytc("div", "GB_loader"), function(elm) { AJS.removeElement(elm) });
  
  //If ie, hide select, in others hide flash
  if(AJS.isIe())
    AJS.map(AJS.$bytc("select"), function(elm) {elm.style.visibility = "hidden"});
  AJS.map(AJS.$bytc("object"), function(elm) {elm.style.visibility = "hidden"});

  this.initOverlayIfNeeded();
  
  this.setOverlayDimension();
  AJS.showElement(this.overlay);
  this.setFullScreenOption();

  this.initIfNeeded();

  AJS.hideElement(this.g_window);

  if(this.type == "page")
    AJS.ACN(this.g_container, this.iframe);
  else {
    this.dummy_holder = AJS.DIV({'class': 'GB_dummy', 'style': 'width: 200px; height: 200px; background-color: #fff;'});
    AJS.ACN(this.g_container, this.dummy_holder);
  }

  if(caption == "")
    caption = "&nbsp;";
  this.div_caption.innerHTML = caption;

  //AJS.showElement(this.g_window)
  Effect.Appear(this.g_window, {duration: 1});

  this.setVerticalPosition();
  this.setTopNLeft();
  this.setWidthNHeight();


  this.showLoader();

  GB_CURRENT.startLoading();

  return false;
}

GreyBox.prototype.hide = function() {
	Effect.Fade(this.g_window, {duration: 0.3});
	Effect.Fade(this.overlay, {duration: 0.3});
//  AJS.hideElement(this.g_window, this.overlay);


  try{ AJS.removeElement(this.iframe); }
  catch(e) {}

  this.iframe = null;

  if(this.type == "image") {
    this.width = 200;
    this.height = 200;
  }

  if(AJS.isIe()) 
    AJS.map(AJS.$bytc("select"), function(elm) {elm.style.visibility = "visible"});
  AJS.map(AJS.$bytc("object"), function(elm) {elm.style.visibility = "visible"});

  if(GB_CURRENT.callback_fn)
    GB_CURRENT.callback_fn();

  GB_CURRENT = null;
}

/** 
  If you only use one instance of GreyBox
  **/
GB_initOneIfNeeded = function() {
  if(!GB_ONLY_ONE) {
    GB_ONLY_ONE = new GreyBox();
    GB_ONLY_ONE.setImageDir(GB_IMG_DIR);
  }
}

GB_show = function(caption, url, /* optional */ height, width, callback_fn) {
  GB_ONLY_ONE.defaultSize();
  GB_ONLY_ONE.setFullScreen(false);
  GB_ONLY_ONE.setType("page");

  GB_ONLY_ONE.setCallback(callback_fn);
  GB_ONLY_ONE.setImageDir(GB_IMG_DIR);
  GB_ONLY_ONE.setDimension(width, height);
  GB_ONLY_ONE.show(caption, url);
  return false;
}

GB_showFullScreen = function(caption, url, /* optional */ callback_fn) {
  GB_ONLY_ONE.defaultSize();
  GB_ONLY_ONE.setType("page");

  GB_ONLY_ONE.setCallback(callback_fn);
  GB_ONLY_ONE.setImageDir(GB_IMG_DIR);
  GB_ONLY_ONE.setFullScreen(true);
  GB_ONLY_ONE.show(caption, url);
  return false;
}

GB_showImage = function(caption, url) {
  GB_ONLY_ONE.defaultSize();
  GB_ONLY_ONE.setFullScreen(false);
  GB_ONLY_ONE.setType("image");

  GB_ONLY_ONE.setImageDir(GB_IMG_DIR);
  GB_ONLY_ONE.show(caption, url);
  return false;
}

GB_hide = function() {
  GB_CURRENT.hide();
}

/**
  Preload all the images used by GreyBox. Static function
  **/
GreyBox.preloadGreyBoxImages = function() {
  var pics = [];
  var fn = function(path) { 
    var pic = new Image();
    pic.src = GB_IMG_DIR + path;
    pics.push(pic);
  };
  AJS.map(['indicator.gif', 'blank.gif', 'close.gif', 'header_bg.gif', 'overlay_light.png', 'overlay_dark.png'], AJS.$b(fn, this));
}


////
// Internal functions
////
GreyBox.prototype.getOverlayImage = function() {
  return "overlay_" + this.overlay_color + ".png";
};

/**
  Init functions
  **/
GreyBox.prototype.initOverlayIfNeeded = function() {
  //Create the overlay
  this.overlay = AJS.DIV({'id': 'GB_overlay'});
  if(AJS.isIe()) {
    this.overlay.style.backgroundColor = "#000000";
    this.overlay.style.backgroundColor = "transparent";
    this.overlay.style.backgroundImage = "url("+ this.img_dir +"blank.gif)";
    this.overlay.runtimeStyle.filter = "progid:DXImageTransform.Microsoft.AlphaImageLoader(src='" + this.img_dir + this.getOverlayImage() + "',sizingMethod='scale')";
  }
  else 
    this.overlay.style.backgroundImage = "url("+ this.img_dir + this.getOverlayImage() +")";

  if(this.overlay_click_close)
    AJS.AEV(this.overlay, "click", GB_hide);

  AJS.getBody().insertBefore(this.overlay, AJS.getBody().firstChild);
};

GreyBox.prototype.initIfNeeded = function() {
  this.init();
  this.setWidthNHeight = AJS.$b(this.setWidthNHeight, this);
  this.setTopNLeft = AJS.$b(this.setTopNLeft, this);
  this.setFullScreenOption = AJS.$b(this.setFullScreenOption, this);
  this.setOverlayDimension = AJS.$b(this.setOverlayDimension, this);

  GreyBox.addOnWinResize(this.setWidthNHeight, this.setTopNLeft, this.setFullScreenOption, this.setOverlayDimension);

  if(this.type == "page")
    this.g_container.style.marginBottom = "-3px";

  var fn = function() { 
    this.setOverlayDimension();
    this.setVerticalPosition(); 
    this.setTopNLeft();
    this.setWidthNHeight(); 
  };
  AJS.AEV(window, "scroll", AJS.$b(fn, this));

  if(!this.iframe) {
    var new_frame;
    var d = {'name': 'GB_frame', 'class': 'GB_frame', 'frameBorder': 0};
    if(this.type == "page") {
      new_frame = AJS.IFRAME(d);
      AJS.hideElement(new_frame);
    }
    else {
     new_frame = new Image();
    }

    this.iframe = new_frame;
  }
}

GreyBox.prototype.init = function() {
  //Create the window
  this.g_window = AJS.DIV({'id': 'GB_window'});

  //Create the table structure
  var table = AJS.TABLE({'class': 'GB_t_frame', 'frameborder': 0});
  var tbody = AJS.TBODY();
  AJS.ACN(table, tbody);

  //Midlle
  var td_middle_m = AJS.TD({'class': 'GB_content'});
  this.td_middle_m = td_middle_m;

  AJS.ACN(tbody, AJS.TR(td_middle_m));

  //Append caption and close
  var header = AJS.TABLE({'class': 'GB_header', 'cellpadding': 0, 'cellspacing': 0});
  this.header = header;

  var img_close = AJS.IMG({'src': this.img_dir + 'close.gif'});
  var close = AJS.SPAN({'class': 'GB_close'}, "Close", img_close);
  AJS.AEV(close, "click", GB_hide);

  var caption = AJS.SPAN({'class': 'GB_caption'});
  this.div_caption = caption;

  var header_row = AJS.TD(caption, close);
//  header.style.backgroundImage = "url("+ this.img_dir +"header_bg.gif)";

  tbody_header = AJS.TBODY();
  AJS.ACN(tbody_header, AJS.TR(header_row));
  AJS.ACN(header, tbody_header);

  AJS.ACN(td_middle_m, header);

  //Container
  this.g_container = AJS.DIV({'class': 'GB_container'});
  AJS.ACN(td_middle_m, this.g_container);

  AJS.ACN(this.g_window, table);

  AJS.getBody().insertBefore(this.g_window, this.overlay.nextSibling);
}

GreyBox.prototype.startLoading = function() {
  //Start preloading the object
  this.iframe.src = this.url;

  if(AJS.isIe()) {
    //IE the stupid bitch - needs custom code for this ARGH
    var check_state = function() {
      if(this.iframe.readyState == "complete")
        GreyBox.loaded();
      else
        AJS.callLater(AJS.$b(check_state, this), 30);
    };
    AJS.callLater(AJS.$b(check_state, this), 30);
  }
  //Safari AND opera has a bug with onload.. bah
  else if(AJS.isSafari() || AJS.isOpera() && this.type == "image") {
    AJS.callLater(GreyBox.loaded, 250);
  }
  else {
    this.iframe.onload = GreyBox.loaded;
  }
}

/**
  Loading functions
  **/
GreyBox.loaded = function() {
  var me = GB_CURRENT;
  GB_CURRENT.iframe.onload = null;

  if(me) {
    AJS.removeElement(me.loader);

    if(me.type == "page") {
      AJS.showElement(me.iframe);
      me.setIframeWidthNHeight();
    }

    if(me.type == "image") {
      var r_img = AJS.IMG({'src': me.url});
      if(GB_CURRENT.image_click_close)
        AJS.AEV(r_img, "click", GB_hide);

      var insert = function() {
        AJS.ACN(GB_CURRENT.g_container, r_img);
        GB_CURRENT.iframe = r_img;
      };
      var count = 0;

      var fn = function() {
        if(count > 10)
          return;
        this.width = this.iframe.width;
        this.height = this.iframe.height;

        if(this.width == 0 || this.height == 0) {
          count++;
          AJS.callLater(AJS.$b(fn, me), 100);
          return;
        }

        //Safari render bugfix
        if(AJS.isSafari())
          this.overlay.style.backgroundColor = "transparent";

        this.setTopNLeft();
        this.setWidthNHeight();
        AJS.removeElement(this.dummy_holder);

        AJS.callLater(AJS.$b(insert, me), 50);

        count++;
      };
      AJS.callLater(AJS.$b(fn, me), 100);
    }
  }
}

GreyBox.prototype.showLoader = function() {
  this.loader = AJS.DIV({'class': 'GB_loader'});
  
  AJS.setWidth(this.loader, this.width);
  AJS.setHeight(this.loader, this.height);

  var indicator = AJS.IMG({'src': this.img_dir + 'indicator.gif'});
  AJS.ACN(this.loader, AJS.BR(), indicator, AJS.BR(), AJS.BR(), AJS.SPAN("LOADING..."));

  if(this.type != "page") {
    AJS.RCN(this.dummy_holder, this.loader);
    AJS.setTop(this.loader, AJS.absolutePosition(this.dummy_holder).y);
  }
  else {
    AJS.ACN(this.g_container, this.loader);
    AJS.setTop(this.loader, AJS.absolutePosition(this.iframe).y);
    AJS.showElement(this.loader);
  }
}

/**
  Set dimension functions
  **/
GreyBox.prototype.setIframeWidthNHeight = function() {
  try{
    AJS.setWidth(this.iframe, this.width);
    AJS.setHeight(this.iframe, this.height);
  }
  catch(e) {
  }
}

GreyBox.prototype.setOverlayDimension = function() {
  var array_page_size = GreyBox.getWindowSize();
  if((navigator.userAgent.toLowerCase().indexOf("firefox") != -1))
   AJS.setWidth(this.overlay, "100%");
  else
   AJS.setWidth(this.overlay, array_page_size[0]);

  var max_height = Math.max(AJS.getScrollTop()+array_page_size[1], AJS.getScrollTop()+this.height);
  if(max_height < AJS.getScrollTop())
    AJS.setHeight(this.overlay, max_height);
  else
    AJS.setHeight(this.overlay, AJS.getScrollTop()+array_page_size[1]);
}

GreyBox.prototype.setWidthNHeight = function() {
  //Set size
  AJS.setWidth(this.g_window, this.width);
  AJS.setHeight(this.g_window, this.height);

  AJS.setWidth(this.g_container, this.width);
  AJS.setHeight(this.g_container, this.height);

  if(this.type == "page")
    this.setIframeWidthNHeight();

  //Set size on components
  AJS.setWidth(this.td_middle_m, this.width);
}

GreyBox.prototype.setTopNLeft = function() {
  var array_page_size = GreyBox.getWindowSize();
  AJS.setLeft(this.g_window, ((array_page_size[0] - this.width)/2)-13);

  if(this.center_window) {
    var fl = ((array_page_size[1] - this.height) /2) - 15;
    AJS.setTop(this.g_window, fl);
  }
  else {
    if(this.g_window.offsetHeight < array_page_size[1])
      AJS.setTop(this.g_window, AJS.getScrollTop());
  }
}

GreyBox.prototype.setVerticalPosition = function() {
  var array_page_size = GreyBox.getWindowSize();
  var st = AJS.getScrollTop();
  if(this.g_window.offsetWidth <= array_page_size[1] || st <= this.g_window.offsetTop) {
    AJS.setTop(this.g_window, st);
  }
}

GreyBox.prototype.setFullScreenOption = function() {
  if(this.full_screen) {
    var array_page_size = GreyBox.getWindowSize();

    overlay_h = array_page_size[1];

    this.width = Math.round(this.overlay.offsetWidth - (this.overlay.offsetWidth/100)*10);
    this.height = Math.round(overlay_h - (overlay_h/100)*10);
  }
}

GreyBox.prototype.defaultSize = function() {
  this.width = 300;
  this.height = 300;
}

////
// Misc.
////
GreyBox.getWindowSize = function() {
	var window_width, window_height;
	if (self.innerHeight) {	
		window_width = self.innerWidth;
		window_height = self.innerHeight;
	} else if (document.documentElement && document.documentElement.clientHeight) { 
		window_width = document.documentElement.clientWidth;
		window_height = document.documentElement.clientHeight;
	} else if (document.body) { 
		window_width = document.body.clientWidth;
		window_height = document.body.clientHeight;
	}	
	return [window_width, window_height];
}

GreyBox.addOnWinResize = function(funcs) {
  funcs = AJS.$A(funcs);
  AJS.map(funcs, function(fn) { AJS.AEV(window, "resize", fn); });
}

GB_ONLY_ONE = new GreyBox();
