/*
	Copyright (c) 2004-2006, The Dojo Foundation
	All Rights Reserved.

	Licensed under the Academic Free License version 2.1 or above OR the
	modified BSD license. For more information on Dojo licensing, see:

		http://dojotoolkit.org/community/licensing.shtml
*/

/*
	This is a compiled version of Dojo, built for deployment and not for
	development. To get an editable version, please visit:

		http://dojotoolkit.org

	for documentation and information on getting the source.
*/

if(typeof dojo=="undefined"){
var dj_global=this;
var dj_currentContext=this;
function dj_undef(_1,_2){
return (typeof (_2||dj_currentContext)[_1]=="undefined");
}
if(dj_undef("djConfig",this)){
var djConfig={};
}
if(dj_undef("dojo",this)){
var dojo={};
}
dojo.global=function(){
return dj_currentContext;
};
dojo.locale=djConfig.locale;
dojo.version={major:0,minor:0,patch:0,flag:"dev",revision:Number("$Rev: 7480 $".match(/[0-9]+/)[0]),toString:function(){
with(dojo.version){
return major+"."+minor+"."+patch+flag+" ("+revision+")";
}
}};
dojo.evalProp=function(_3,_4,_5){
if((!_4)||(!_3)){
return undefined;
}
if(!dj_undef(_3,_4)){
return _4[_3];
}
return (_5?(_4[_3]={}):undefined);
};
dojo.parseObjPath=function(_6,_7,_8){
var _9=(_7||dojo.global());
var _a=_6.split(".");
var _b=_a.pop();
for(var i=0,l=_a.length;i<l&&_9;i++){
_9=dojo.evalProp(_a[i],_9,_8);
}
return {obj:_9,prop:_b};
};
dojo.evalObjPath=function(_e,_f){
if(typeof _e!="string"){
return dojo.global();
}
if(_e.indexOf(".")==-1){
return dojo.evalProp(_e,dojo.global(),_f);
}
var ref=dojo.parseObjPath(_e,dojo.global(),_f);
if(ref){
return dojo.evalProp(ref.prop,ref.obj,_f);
}
return null;
};
dojo.errorToString=function(_11){
if(!dj_undef("message",_11)){
return _11.message;
}else{
if(!dj_undef("description",_11)){
return _11.description;
}else{
return _11;
}
}
};
dojo.raise=function(_12,_13){
if(_13){
_12=_12+": "+dojo.errorToString(_13);
}else{
_12=dojo.errorToString(_12);
}
try{
if(djConfig.isDebug){
dojo.hostenv.println("FATAL exception raised: "+_12);
}
}
catch(e){
}
throw _13||Error(_12);
};
dojo.debug=function(){
};
dojo.debugShallow=function(obj){
};
dojo.profile={start:function(){
},end:function(){
},stop:function(){
},dump:function(){
}};
function dj_eval(_15){
return dj_global.eval?dj_global.eval(_15):eval(_15);
}
dojo.unimplemented=function(_16,_17){
var _18="'"+_16+"' not implemented";
if(_17!=null){
_18+=" "+_17;
}
dojo.raise(_18);
};
dojo.deprecated=function(_19,_1a,_1b){
var _1c="DEPRECATED: "+_19;
if(_1a){
_1c+=" "+_1a;
}
if(_1b){
_1c+=" -- will be removed in version: "+_1b;
}
dojo.debug(_1c);
};
dojo.render=(function(){
function vscaffold(_1d,_1e){
var tmp={capable:false,support:{builtin:false,plugin:false},prefixes:_1d};
for(var i=0;i<_1e.length;i++){
tmp[_1e[i]]=false;
}
return tmp;
}
return {name:"",ver:dojo.version,os:{win:false,linux:false,osx:false},html:vscaffold(["html"],["ie","opera","khtml","safari","moz"]),svg:vscaffold(["svg"],["corel","adobe","batik"]),vml:vscaffold(["vml"],["ie"]),swf:vscaffold(["Swf","Flash","Mm"],["mm"]),swt:vscaffold(["Swt"],["ibm"])};
})();
dojo.hostenv=(function(){
var _21={isDebug:false,allowQueryConfig:false,baseScriptUri:"",baseRelativePath:"",libraryScriptUri:"",iePreventClobber:false,ieClobberMinimal:true,preventBackButtonFix:true,delayMozLoadingFix:false,searchIds:[],parseWidgets:true};
if(typeof djConfig=="undefined"){
djConfig=_21;
}else{
for(var _22 in _21){
if(typeof djConfig[_22]=="undefined"){
djConfig[_22]=_21[_22];
}
}
}
return {name_:"(unset)",version_:"(unset)",getName:function(){
return this.name_;
},getVersion:function(){
return this.version_;
},getText:function(uri){
dojo.unimplemented("getText","uri="+uri);
}};
})();
dojo.hostenv.getBaseScriptUri=function(){
if(djConfig.baseScriptUri.length){
return djConfig.baseScriptUri;
}
var uri=new String(djConfig.libraryScriptUri||djConfig.baseRelativePath);
if(!uri){
dojo.raise("Nothing returned by getLibraryScriptUri(): "+uri);
}
var _25=uri.lastIndexOf("/");
djConfig.baseScriptUri=djConfig.baseRelativePath;
return djConfig.baseScriptUri;
};
(function(){
var _26={pkgFileName:"__package__",loading_modules_:{},loaded_modules_:{},addedToLoadingCount:[],removedFromLoadingCount:[],inFlightCount:0,modulePrefixes_:{dojo:{name:"dojo",value:"src"}},setModulePrefix:function(_27,_28){
this.modulePrefixes_[_27]={name:_27,value:_28};
},moduleHasPrefix:function(_29){
var mp=this.modulePrefixes_;
return Boolean(mp[_29]&&mp[_29].value);
},getModulePrefix:function(_2b){
if(this.moduleHasPrefix(_2b)){
return this.modulePrefixes_[_2b].value;
}
return _2b;
},getTextStack:[],loadUriStack:[],loadedUris:[],post_load_:false,modulesLoadedListeners:[],unloadListeners:[],loadNotifying:false};
for(var _2c in _26){
dojo.hostenv[_2c]=_26[_2c];
}
})();
dojo.hostenv.loadPath=function(_2d,_2e,cb){
var uri;
if(_2d.charAt(0)=="/"||_2d.match(/^\w+:/)){
uri=_2d;
}else{
uri=this.getBaseScriptUri()+_2d;
}
if(djConfig.cacheBust&&dojo.render.html.capable){
uri+="?"+String(djConfig.cacheBust).replace(/\W+/g,"");
}
try{
return !_2e?this.loadUri(uri,cb):this.loadUriAndCheck(uri,_2e,cb);
}
catch(e){
dojo.debug(e);
return false;
}
};
dojo.hostenv.loadUri=function(uri,cb){
if(this.loadedUris[uri]){
return true;
}
var _33=this.getText(uri,null,true);
if(!_33){
return false;
}
this.loadedUris[uri]=true;
if(cb){
_33="("+_33+")";
}
var _34=dj_eval(_33);
if(cb){
cb(_34);
}
return true;
};
dojo.hostenv.loadUriAndCheck=function(uri,_36,cb){
var ok=true;
try{
ok=this.loadUri(uri,cb);
}
catch(e){
dojo.debug("failed loading ",uri," with error: ",e);
}
return Boolean(ok&&this.findModule(_36,false));
};
dojo.loaded=function(){
};
dojo.unloaded=function(){
};
dojo.hostenv.loaded=function(){
this.loadNotifying=true;
this.post_load_=true;
var mll=this.modulesLoadedListeners;
for(var x=0;x<mll.length;x++){
mll[x]();
}
this.modulesLoadedListeners=[];
this.loadNotifying=false;
dojo.loaded();
};
dojo.hostenv.unloaded=function(){
var mll=this.unloadListeners;
while(mll.length){
(mll.pop())();
}
dojo.unloaded();
};
dojo.addOnLoad=function(obj,_3d){
var dh=dojo.hostenv;
if(arguments.length==1){
dh.modulesLoadedListeners.push(obj);
}else{
if(arguments.length>1){
dh.modulesLoadedListeners.push(function(){
obj[_3d]();
});
}
}
if(dh.post_load_&&dh.inFlightCount==0&&!dh.loadNotifying){
dh.callLoaded();
}
};
dojo.addOnUnload=function(obj,_40){
var dh=dojo.hostenv;
if(arguments.length==1){
dh.unloadListeners.push(obj);
}else{
if(arguments.length>1){
dh.unloadListeners.push(function(){
obj[_40]();
});
}
}
};
dojo.hostenv.modulesLoaded=function(){
if(this.post_load_){
return;
}
if(this.loadUriStack.length==0&&this.getTextStack.length==0){
if(this.inFlightCount>0){
dojo.debug("files still in flight!");
return;
}
dojo.hostenv.callLoaded();
}
};
dojo.hostenv.callLoaded=function(){
if(typeof setTimeout=="object"){
setTimeout("dojo.hostenv.loaded();",0);
}else{
dojo.hostenv.loaded();
}
};
dojo.hostenv.getModuleSymbols=function(_42){
var _43=_42.split(".");
for(var i=_43.length;i>0;i--){
var _45=_43.slice(0,i).join(".");
if((i==1)&&!this.moduleHasPrefix(_45)){
_43[0]="../"+_43[0];
}else{
var _46=this.getModulePrefix(_45);
if(_46!=_45){
_43.splice(0,i,_46);
break;
}
}
}
return _43;
};
dojo.hostenv._global_omit_module_check=false;
dojo.hostenv.loadModule=function(_47,_48,_49){
if(!_47){
return;
}
_49=this._global_omit_module_check||_49;
var _4a=this.findModule(_47,false);
if(_4a){
return _4a;
}
if(dj_undef(_47,this.loading_modules_)){
this.addedToLoadingCount.push(_47);
}
this.loading_modules_[_47]=1;
var _4b=_47.replace(/\./g,"/")+".js";
var _4c=_47.split(".");
var _4d=this.getModuleSymbols(_47);
var _4e=((_4d[0].charAt(0)!="/")&&!_4d[0].match(/^\w+:/));
var _4f=_4d[_4d.length-1];
var ok;
if(_4f=="*"){
_47=_4c.slice(0,-1).join(".");
while(_4d.length){
_4d.pop();
_4d.push(this.pkgFileName);
_4b=_4d.join("/")+".js";
if(_4e&&_4b.charAt(0)=="/"){
_4b=_4b.slice(1);
}
ok=this.loadPath(_4b,!_49?_47:null);
if(ok){
break;
}
_4d.pop();
}
}else{
_4b=_4d.join("/")+".js";
_47=_4c.join(".");
var _51=!_49?_47:null;
ok=this.loadPath(_4b,_51);
if(!ok&&!_48){
_4d.pop();
while(_4d.length){
_4b=_4d.join("/")+".js";
ok=this.loadPath(_4b,_51);
if(ok){
break;
}
_4d.pop();
_4b=_4d.join("/")+"/"+this.pkgFileName+".js";
if(_4e&&_4b.charAt(0)=="/"){
_4b=_4b.slice(1);
}
ok=this.loadPath(_4b,_51);
if(ok){
break;
}
}
}
if(!ok&&!_49){
dojo.raise("Could not load '"+_47+"'; last tried '"+_4b+"'");
}
}
if(!_49&&!this["isXDomain"]){
_4a=this.findModule(_47,false);
if(!_4a){
dojo.raise("symbol '"+_47+"' is not defined after loading '"+_4b+"'");
}
}
return _4a;
};
dojo.hostenv.startPackage=function(_52){
var _53=String(_52);
var _54=_53;
var _55=_52.split(/\./);
if(_55[_55.length-1]=="*"){
_55.pop();
_54=_55.join(".");
}
var _56=dojo.evalObjPath(_54,true);
this.loaded_modules_[_53]=_56;
this.loaded_modules_[_54]=_56;
return _56;
};
dojo.hostenv.findModule=function(_57,_58){
var lmn=String(_57);
if(this.loaded_modules_[lmn]){
return this.loaded_modules_[lmn];
}
if(_58){
dojo.raise("no loaded module named '"+_57+"'");
}
return null;
};
dojo.kwCompoundRequire=function(_5a){
var _5b=_5a["common"]||[];
var _5c=_5a[dojo.hostenv.name_]?_5b.concat(_5a[dojo.hostenv.name_]||[]):_5b.concat(_5a["default"]||[]);
for(var x=0;x<_5c.length;x++){
var _5e=_5c[x];
if(_5e.constructor==Array){
dojo.hostenv.loadModule.apply(dojo.hostenv,_5e);
}else{
dojo.hostenv.loadModule(_5e);
}
}
};
dojo.require=function(_5f){
dojo.hostenv.loadModule.apply(dojo.hostenv,arguments);
};
dojo.requireIf=function(_60,_61){
var _62=arguments[0];
if((_62===true)||(_62=="common")||(_62&&dojo.render[_62].capable)){
var _63=[];
for(var i=1;i<arguments.length;i++){
_63.push(arguments[i]);
}
dojo.require.apply(dojo,_63);
}
};
dojo.requireAfterIf=dojo.requireIf;
dojo.provide=function(_65){
return dojo.hostenv.startPackage.apply(dojo.hostenv,arguments);
};
dojo.registerModulePath=function(_66,_67){
return dojo.hostenv.setModulePrefix(_66,_67);
};
if(djConfig["modulePaths"]){
for(var param in djConfig["modulePaths"]){
dojo.registerModulePath(param,djConfig["modulePaths"][param]);
}
}
dojo.setModulePrefix=function(_68,_69){
dojo.deprecated("dojo.setModulePrefix(\""+_68+"\", \""+_69+"\")","replaced by dojo.registerModulePath","0.5");
return dojo.registerModulePath(_68,_69);
};
dojo.exists=function(obj,_6b){
var p=_6b.split(".");
for(var i=0;i<p.length;i++){
if(!obj[p[i]]){
return false;
}
obj=obj[p[i]];
}
return true;
};
dojo.hostenv.normalizeLocale=function(_6e){
var _6f=_6e?_6e.toLowerCase():dojo.locale;
if(_6f=="root"){
_6f="ROOT";
}
return _6f;
};
dojo.hostenv.searchLocalePath=function(_70,_71,_72){
_70=dojo.hostenv.normalizeLocale(_70);
var _73=_70.split("-");
var _74=[];
for(var i=_73.length;i>0;i--){
_74.push(_73.slice(0,i).join("-"));
}
_74.push(false);
if(_71){
_74.reverse();
}
for(var j=_74.length-1;j>=0;j--){
var loc=_74[j]||"ROOT";
var _78=_72(loc);
if(_78){
break;
}
}
};
dojo.hostenv.localesGenerated;
dojo.hostenv.registerNlsPrefix=function(){
dojo.registerModulePath("nls","nls");
};
dojo.hostenv.preloadLocalizations=function(){
if(dojo.hostenv.localesGenerated){
dojo.hostenv.registerNlsPrefix();
function preload(_79){
_79=dojo.hostenv.normalizeLocale(_79);
dojo.hostenv.searchLocalePath(_79,true,function(loc){
for(var i=0;i<dojo.hostenv.localesGenerated.length;i++){
if(dojo.hostenv.localesGenerated[i]==loc){
dojo["require"]("nls.dojo_"+loc);
return true;
}
}
return false;
});
}
preload();
var _7c=djConfig.extraLocale||[];
for(var i=0;i<_7c.length;i++){
preload(_7c[i]);
}
}
dojo.hostenv.preloadLocalizations=function(){
};
};
dojo.requireLocalization=function(_7e,_7f,_80,_81){
dojo.hostenv.preloadLocalizations();
var _82=dojo.hostenv.normalizeLocale(_80);
var _83=[_7e,"nls",_7f].join(".");
var _84="";
if(_81){
var _85=_81.split(",");
for(var i=0;i<_85.length;i++){
if(_82.indexOf(_85[i])==0){
if(_85[i].length>_84.length){
_84=_85[i];
}
}
}
if(!_84){
_84="ROOT";
}
}
var _87=_81?_84:_82;
var _88=dojo.hostenv.findModule(_83);
var _89=null;
if(_88){
if(djConfig.localizationComplete&&_88._built){
return;
}
var _8a=_87.replace("-","_");
var _8b=_83+"."+_8a;
_89=dojo.hostenv.findModule(_8b);
}
if(!_89){
_88=dojo.hostenv.startPackage(_83);
var _8c=dojo.hostenv.getModuleSymbols(_7e);
var _8d=_8c.concat("nls").join("/");
var _8e;
dojo.hostenv.searchLocalePath(_87,_81,function(loc){
var _90=loc.replace("-","_");
var _91=_83+"."+_90;
var _92=false;
if(!dojo.hostenv.findModule(_91)){
dojo.hostenv.startPackage(_91);
var _93=[_8d];
if(loc!="ROOT"){
_93.push(loc);
}
_93.push(_7f);
var _94=_93.join("/")+".js";
_92=dojo.hostenv.loadPath(_94,null,function(_95){
var _96=function(){
};
_96.prototype=_8e;
_88[_90]=new _96();
for(var j in _95){
_88[_90][j]=_95[j];
}
});
}else{
_92=true;
}
if(_92&&_88[_90]){
_8e=_88[_90];
}else{
_88[_90]=_8e;
}
if(_81){
return true;
}
});
}
if(_81&&_82!=_84){
_88[_82.replace("-","_")]=_88[_84.replace("-","_")];
}
};
(function(){
var _98=djConfig.extraLocale;
if(_98){
if(!_98 instanceof Array){
_98=[_98];
}
var req=dojo.requireLocalization;
dojo.requireLocalization=function(m,b,_9c,_9d){
req(m,b,_9c,_9d);
if(_9c){
return;
}
for(var i=0;i<_98.length;i++){
req(m,b,_98[i],_9d);
}
};
}
})();
}
if(typeof window!="undefined"){
(function(){
if(djConfig.allowQueryConfig){
var _9f=document.location.toString();
var _a0=_9f.split("?",2);
if(_a0.length>1){
var _a1=_a0[1];
var _a2=_a1.split("&");
for(var x in _a2){
var sp=_a2[x].split("=");
if((sp[0].length>9)&&(sp[0].substr(0,9)=="djConfig.")){
var opt=sp[0].substr(9);
try{
djConfig[opt]=eval(sp[1]);
}
catch(e){
djConfig[opt]=sp[1];
}
}
}
}
}
if(((djConfig["baseScriptUri"]=="")||(djConfig["baseRelativePath"]==""))&&(document&&document.getElementsByTagName)){
var _a6=document.getElementsByTagName("script");
var _a7=/(__package__|dojo|bootstrap1)\.js([\?\.]|$)/i;
for(var i=0;i<_a6.length;i++){
var src=_a6[i].getAttribute("src");
if(!src){
continue;
}
var m=src.match(_a7);
if(m){
var _ab=src.substring(0,m.index);
if(src.indexOf("bootstrap1")>-1){
_ab+="../";
}
if(!this["djConfig"]){
djConfig={};
}
if(djConfig["baseScriptUri"]==""){
djConfig["baseScriptUri"]=_ab;
}
if(djConfig["baseRelativePath"]==""){
djConfig["baseRelativePath"]=_ab;
}
break;
}
}
}
var dr=dojo.render;
var drh=dojo.render.html;
var drs=dojo.render.svg;
var dua=(drh.UA=navigator.userAgent);
var dav=(drh.AV=navigator.appVersion);
var t=true;
var f=false;
drh.capable=t;
drh.support.builtin=t;
dr.ver=parseFloat(drh.AV);
dr.os.mac=dav.indexOf("Macintosh")>=0;
dr.os.win=dav.indexOf("Windows")>=0;
dr.os.linux=dav.indexOf("X11")>=0;
drh.opera=dua.indexOf("Opera")>=0;
drh.khtml=(dav.indexOf("Konqueror")>=0)||(dav.indexOf("Safari")>=0);
drh.safari=dav.indexOf("Safari")>=0;
var _b3=dua.indexOf("Gecko");
drh.mozilla=drh.moz=(_b3>=0)&&(!drh.khtml);
if(drh.mozilla){
drh.geckoVersion=dua.substring(_b3+6,_b3+14);
}
drh.ie=(document.all)&&(!drh.opera);
drh.ie50=drh.ie&&dav.indexOf("MSIE 5.0")>=0;
drh.ie55=drh.ie&&dav.indexOf("MSIE 5.5")>=0;
drh.ie60=drh.ie&&dav.indexOf("MSIE 6.0")>=0;
drh.ie70=drh.ie&&dav.indexOf("MSIE 7.0")>=0;
var cm=document["compatMode"];
drh.quirks=(cm=="BackCompat")||(cm=="QuirksMode")||drh.ie55||drh.ie50;
dojo.locale=dojo.locale||(drh.ie?navigator.userLanguage:navigator.language).toLowerCase();
dr.vml.capable=drh.ie;
drs.capable=f;
drs.support.plugin=f;
drs.support.builtin=f;
var _b5=window["document"];
var tdi=_b5["implementation"];
if((tdi)&&(tdi["hasFeature"])&&(tdi.hasFeature("org.w3c.dom.svg","1.0"))){
drs.capable=t;
drs.support.builtin=t;
drs.support.plugin=f;
}
if(drh.safari){
var tmp=dua.split("AppleWebKit/")[1];
var ver=parseFloat(tmp.split(" ")[0]);
if(ver>=420){
drs.capable=t;
drs.support.builtin=t;
drs.support.plugin=f;
}
}else{
}
})();
dojo.hostenv.startPackage("dojo.hostenv");
dojo.render.name=dojo.hostenv.name_="browser";
dojo.hostenv.searchIds=[];
dojo.hostenv._XMLHTTP_PROGIDS=["Msxml2.XMLHTTP","Microsoft.XMLHTTP","Msxml2.XMLHTTP.4.0"];
dojo.hostenv.getXmlhttpObject=function(){
var _b9=null;
var _ba=null;
try{
_b9=new XMLHttpRequest();
}
catch(e){
}
if(!_b9){
for(var i=0;i<3;++i){
var _bc=dojo.hostenv._XMLHTTP_PROGIDS[i];
try{
_b9=new ActiveXObject(_bc);
}
catch(e){
_ba=e;
}
if(_b9){
dojo.hostenv._XMLHTTP_PROGIDS=[_bc];
break;
}
}
}
if(!_b9){
return dojo.raise("XMLHTTP not available",_ba);
}
return _b9;
};
dojo.hostenv._blockAsync=false;
dojo.hostenv.getText=function(uri,_be,_bf){
if(!_be){
this._blockAsync=true;
}
var _c0=this.getXmlhttpObject();
function isDocumentOk(_c1){
var _c2=_c1["status"];
return Boolean((!_c2)||((200<=_c2)&&(300>_c2))||(_c2==304));
}
if(_be){
var _c3=this,_c4=null,gbl=dojo.global();
var xhr=dojo.evalObjPath("dojo.io.XMLHTTPTransport");
_c0.onreadystatechange=function(){
if(_c4){
gbl.clearTimeout(_c4);
_c4=null;
}
if(_c3._blockAsync||(xhr&&xhr._blockAsync)){
_c4=gbl.setTimeout(function(){
_c0.onreadystatechange.apply(this);
},10);
}else{
if(4==_c0.readyState){
if(isDocumentOk(_c0)){
_be(_c0.responseText);
}
}
}
};
}
_c0.open("GET",uri,_be?true:false);
try{
_c0.send(null);
if(_be){
return null;
}
if(!isDocumentOk(_c0)){
var err=Error("Unable to load "+uri+" status:"+_c0.status);
err.status=_c0.status;
err.responseText=_c0.responseText;
throw err;
}
}
catch(e){
this._blockAsync=false;
if((_bf)&&(!_be)){
return null;
}else{
throw e;
}
}
this._blockAsync=false;
return _c0.responseText;
};
dojo.hostenv.defaultDebugContainerId="dojoDebug";
dojo.hostenv._println_buffer=[];
dojo.hostenv._println_safe=false;
dojo.hostenv.println=function(_c8){
if(!dojo.hostenv._println_safe){
dojo.hostenv._println_buffer.push(_c8);
}else{
try{
var _c9=document.getElementById(djConfig.debugContainerId?djConfig.debugContainerId:dojo.hostenv.defaultDebugContainerId);
if(!_c9){
_c9=dojo.body();
}
var div=document.createElement("div");
div.appendChild(document.createTextNode(_c8));
_c9.appendChild(div);
}
catch(e){
try{
document.write("<div>"+_c8+"</div>");
}
catch(e2){
window.status=_c8;
}
}
}
};
dojo.addOnLoad(function(){
dojo.hostenv._println_safe=true;
while(dojo.hostenv._println_buffer.length>0){
dojo.hostenv.println(dojo.hostenv._println_buffer.shift());
}
});
function dj_addNodeEvtHdlr(_cb,_cc,fp){
var _ce=_cb["on"+_cc]||function(){
};
_cb["on"+_cc]=function(){
fp.apply(_cb,arguments);
_ce.apply(_cb,arguments);
};
return true;
}
function dj_load_init(e){
var _d0=(e&&e.type)?e.type.toLowerCase():"load";
if(arguments.callee.initialized||(_d0!="domcontentloaded"&&_d0!="load")){
return;
}
arguments.callee.initialized=true;
if(typeof (_timer)!="undefined"){
clearInterval(_timer);
delete _timer;
}
var _d1=function(){
if(dojo.render.html.ie){
dojo.hostenv.makeWidgets();
}
};
if(dojo.hostenv.inFlightCount==0){
_d1();
dojo.hostenv.modulesLoaded();
}else{
dojo.hostenv.modulesLoadedListeners.unshift(_d1);
}
}
if(document.addEventListener){
if(dojo.render.html.opera||(dojo.render.html.moz&&!djConfig.delayMozLoadingFix)){
document.addEventListener("DOMContentLoaded",dj_load_init,null);
}
window.addEventListener("load",dj_load_init,null);
}
if(dojo.render.html.ie&&dojo.render.os.win){
document.attachEvent("onreadystatechange",function(e){
if(document.readyState=="complete"){
dj_load_init();
}
});
}
if(/(WebKit|khtml)/i.test(navigator.userAgent)){
var _timer=setInterval(function(){
if(/loaded|complete/.test(document.readyState)){
dj_load_init();
}
},10);
}
if(dojo.render.html.ie){
dj_addNodeEvtHdlr(window,"beforeunload",function(){
dojo.hostenv._unloading=true;
window.setTimeout(function(){
dojo.hostenv._unloading=false;
},0);
});
}
dj_addNodeEvtHdlr(window,"unload",function(){
dojo.hostenv.unloaded();
if((!dojo.render.html.ie)||(dojo.render.html.ie&&dojo.hostenv._unloading)){
dojo.hostenv.unloaded();
}
});
dojo.hostenv.makeWidgets=function(){
var _d3=[];
if(djConfig.searchIds&&djConfig.searchIds.length>0){
_d3=_d3.concat(djConfig.searchIds);
}
if(dojo.hostenv.searchIds&&dojo.hostenv.searchIds.length>0){
_d3=_d3.concat(dojo.hostenv.searchIds);
}
if((djConfig.parseWidgets)||(_d3.length>0)){
if(dojo.evalObjPath("dojo.widget.Parse")){
var _d4=new dojo.xml.Parse();
if(_d3.length>0){
for(var x=0;x<_d3.length;x++){
var _d6=document.getElementById(_d3[x]);
if(!_d6){
continue;
}
var _d7=_d4.parseElement(_d6,null,true);
dojo.widget.getParser().createComponents(_d7);
}
}else{
if(djConfig.parseWidgets){
var _d7=_d4.parseElement(dojo.body(),null,true);
dojo.widget.getParser().createComponents(_d7);
}
}
}
}
};
dojo.addOnLoad(function(){
if(!dojo.render.html.ie){
dojo.hostenv.makeWidgets();
}
});
try{
if(dojo.render.html.ie){
document.namespaces.add("v","urn:schemas-microsoft-com:vml");
document.createStyleSheet().addRule("v\\:*","behavior:url(#default#VML)");
}
}
catch(e){
}
dojo.hostenv.writeIncludes=function(){
};
if(!dj_undef("document",this)){
dj_currentDocument=this.document;
}
dojo.doc=function(){
return dj_currentDocument;
};
dojo.body=function(){
return dojo.doc().body||dojo.doc().getElementsByTagName("body")[0];
};
dojo.byId=function(id,doc){
if((id)&&((typeof id=="string")||(id instanceof String))){
if(!doc){
doc=dj_currentDocument;
}
var ele=doc.getElementById(id);
if(ele&&(ele.id!=id)&&doc.all){
ele=null;
eles=doc.all[id];
if(eles){
if(eles.length){
for(var i=0;i<eles.length;i++){
if(eles[i].id==id){
ele=eles[i];
break;
}
}
}else{
ele=eles;
}
}
}
return ele;
}
return id;
};
dojo.setContext=function(_dc,_dd){
dj_currentContext=_dc;
dj_currentDocument=_dd;
};
dojo._fireCallback=function(_de,_df,_e0){
if((_df)&&((typeof _de=="string")||(_de instanceof String))){
_de=_df[_de];
}
return (_df?_de.apply(_df,_e0||[]):_de());
};
dojo.withGlobal=function(_e1,_e2,_e3,_e4){
var _e5;
var _e6=dj_currentContext;
var _e7=dj_currentDocument;
try{
dojo.setContext(_e1,_e1.document);
_e5=dojo._fireCallback(_e2,_e3,_e4);
}
finally{
dojo.setContext(_e6,_e7);
}
return _e5;
};
dojo.withDoc=function(_e8,_e9,_ea,_eb){
var _ec;
var _ed=dj_currentDocument;
try{
dj_currentDocument=_e8;
_ec=dojo._fireCallback(_e9,_ea,_eb);
}
finally{
dj_currentDocument=_ed;
}
return _ec;
};
}
dojo.requireIf((djConfig["isDebug"]||djConfig["debugAtAllCosts"]),"dojo.debug");
dojo.requireIf(djConfig["debugAtAllCosts"]&&!window.widget&&!djConfig["useXDomain"],"dojo.browser_debug");
dojo.requireIf(djConfig["debugAtAllCosts"]&&!window.widget&&djConfig["useXDomain"],"dojo.browser_debug_xd");
dojo.provide("dojo.dom");
dojo.dom.ELEMENT_NODE=1;
dojo.dom.ATTRIBUTE_NODE=2;
dojo.dom.TEXT_NODE=3;
dojo.dom.CDATA_SECTION_NODE=4;
dojo.dom.ENTITY_REFERENCE_NODE=5;
dojo.dom.ENTITY_NODE=6;
dojo.dom.PROCESSING_INSTRUCTION_NODE=7;
dojo.dom.COMMENT_NODE=8;
dojo.dom.DOCUMENT_NODE=9;
dojo.dom.DOCUMENT_TYPE_NODE=10;
dojo.dom.DOCUMENT_FRAGMENT_NODE=11;
dojo.dom.NOTATION_NODE=12;
dojo.dom.dojoml="http://www.dojotoolkit.org/2004/dojoml";
dojo.dom.xmlns={svg:"http://www.w3.org/2000/svg",smil:"http://www.w3.org/2001/SMIL20/",mml:"http://www.w3.org/1998/Math/MathML",cml:"http://www.xml-cml.org",xlink:"http://www.w3.org/1999/xlink",xhtml:"http://www.w3.org/1999/xhtml",xul:"http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul",xbl:"http://www.mozilla.org/xbl",fo:"http://www.w3.org/1999/XSL/Format",xsl:"http://www.w3.org/1999/XSL/Transform",xslt:"http://www.w3.org/1999/XSL/Transform",xi:"http://www.w3.org/2001/XInclude",xforms:"http://www.w3.org/2002/01/xforms",saxon:"http://icl.com/saxon",xalan:"http://xml.apache.org/xslt",xsd:"http://www.w3.org/2001/XMLSchema",dt:"http://www.w3.org/2001/XMLSchema-datatypes",xsi:"http://www.w3.org/2001/XMLSchema-instance",rdf:"http://www.w3.org/1999/02/22-rdf-syntax-ns#",rdfs:"http://www.w3.org/2000/01/rdf-schema#",dc:"http://purl.org/dc/elements/1.1/",dcq:"http://purl.org/dc/qualifiers/1.0","soap-env":"http://schemas.xmlsoap.org/soap/envelope/",wsdl:"http://schemas.xmlsoap.org/wsdl/",AdobeExtensions:"http://ns.adobe.com/AdobeSVGViewerExtensions/3.0/"};
dojo.dom.isNode=function(wh){
if(typeof Element=="function"){
try{
return wh instanceof Element;
}
catch(e){
}
}else{
return wh&&!isNaN(wh.nodeType);
}
};
dojo.dom.getUniqueId=function(){
var _ef=dojo.doc();
do{
var id="dj_unique_"+(++arguments.callee._idIncrement);
}while(_ef.getElementById(id));
return id;
};
dojo.dom.getUniqueId._idIncrement=0;
dojo.dom.firstElement=dojo.dom.getFirstChildElement=function(_f1,_f2){
var _f3=_f1.firstChild;
while(_f3&&_f3.nodeType!=dojo.dom.ELEMENT_NODE){
_f3=_f3.nextSibling;
}
if(_f2&&_f3&&_f3.tagName&&_f3.tagName.toLowerCase()!=_f2.toLowerCase()){
_f3=dojo.dom.nextElement(_f3,_f2);
}
return _f3;
};
dojo.dom.lastElement=dojo.dom.getLastChildElement=function(_f4,_f5){
var _f6=_f4.lastChild;
while(_f6&&_f6.nodeType!=dojo.dom.ELEMENT_NODE){
_f6=_f6.previousSibling;
}
if(_f5&&_f6&&_f6.tagName&&_f6.tagName.toLowerCase()!=_f5.toLowerCase()){
_f6=dojo.dom.prevElement(_f6,_f5);
}
return _f6;
};
dojo.dom.nextElement=dojo.dom.getNextSiblingElement=function(_f7,_f8){
if(!_f7){
return null;
}
do{
_f7=_f7.nextSibling;
}while(_f7&&_f7.nodeType!=dojo.dom.ELEMENT_NODE);
if(_f7&&_f8&&_f8.toLowerCase()!=_f7.tagName.toLowerCase()){
return dojo.dom.nextElement(_f7,_f8);
}
return _f7;
};
dojo.dom.prevElement=dojo.dom.getPreviousSiblingElement=function(_f9,_fa){
if(!_f9){
return null;
}
if(_fa){
_fa=_fa.toLowerCase();
}
do{
_f9=_f9.previousSibling;
}while(_f9&&_f9.nodeType!=dojo.dom.ELEMENT_NODE);
if(_f9&&_fa&&_fa.toLowerCase()!=_f9.tagName.toLowerCase()){
return dojo.dom.prevElement(_f9,_fa);
}
return _f9;
};
dojo.dom.moveChildren=function(_fb,_fc,_fd){
var _fe=0;
if(_fd){
while(_fb.hasChildNodes()&&_fb.firstChild.nodeType==dojo.dom.TEXT_NODE){
_fb.removeChild(_fb.firstChild);
}
while(_fb.hasChildNodes()&&_fb.lastChild.nodeType==dojo.dom.TEXT_NODE){
_fb.removeChild(_fb.lastChild);
}
}
while(_fb.hasChildNodes()){
_fc.appendChild(_fb.firstChild);
_fe++;
}
return _fe;
};
dojo.dom.copyChildren=function(_ff,_100,trim){
var _102=_ff.cloneNode(true);
return this.moveChildren(_102,_100,trim);
};
dojo.dom.replaceChildren=function(node,_104){
var _105=[];
if(dojo.render.html.ie){
for(var i=0;i<node.childNodes.length;i++){
_105.push(node.childNodes[i]);
}
}
dojo.dom.removeChildren(node);
node.appendChild(_104);
for(var i=0;i<_105.length;i++){
dojo.dom.destroyNode(_105[i]);
}
};
dojo.dom.removeChildren=function(node){
var _108=node.childNodes.length;
while(node.hasChildNodes()){
dojo.dom.removeNode(node.firstChild);
}
return _108;
};
dojo.dom.replaceNode=function(node,_10a){
return node.parentNode.replaceChild(_10a,node);
};
dojo.dom.destroyNode=function(node){
if(node.parentNode){
node=dojo.dom.removeNode(node);
}
if(node.nodeType!=3){
if(dojo.evalObjPath("dojo.event.browser.clean",false)){
dojo.event.browser.clean(node);
}
if(dojo.render.html.ie){
node.outerHTML="";
}
}
};
dojo.dom.removeNode=function(node){
if(node&&node.parentNode){
return node.parentNode.removeChild(node);
}
};
dojo.dom.getAncestors=function(node,_10e,_10f){
var _110=[];
var _111=(_10e&&(_10e instanceof Function||typeof _10e=="function"));
while(node){
if(!_111||_10e(node)){
_110.push(node);
}
if(_10f&&_110.length>0){
return _110[0];
}
node=node.parentNode;
}
if(_10f){
return null;
}
return _110;
};
dojo.dom.getAncestorsByTag=function(node,tag,_114){
tag=tag.toLowerCase();
return dojo.dom.getAncestors(node,function(el){
return ((el.tagName)&&(el.tagName.toLowerCase()==tag));
},_114);
};
dojo.dom.getFirstAncestorByTag=function(node,tag){
return dojo.dom.getAncestorsByTag(node,tag,true);
};
dojo.dom.isDescendantOf=function(node,_119,_11a){
if(_11a&&node){
node=node.parentNode;
}
while(node){
if(node==_119){
return true;
}
node=node.parentNode;
}
return false;
};
dojo.dom.innerXML=function(node){
if(node.innerXML){
return node.innerXML;
}else{
if(node.xml){
return node.xml;
}else{
if(typeof XMLSerializer!="undefined"){
return (new XMLSerializer()).serializeToString(node);
}
}
}
};
dojo.dom.createDocument=function(){
var doc=null;
var _11d=dojo.doc();
if(!dj_undef("ActiveXObject")){
var _11e=["MSXML2","Microsoft","MSXML","MSXML3"];
for(var i=0;i<_11e.length;i++){
try{
doc=new ActiveXObject(_11e[i]+".XMLDOM");
}
catch(e){
}
if(doc){
break;
}
}
}else{
if((_11d.implementation)&&(_11d.implementation.createDocument)){
doc=_11d.implementation.createDocument("","",null);
}
}
return doc;
};
dojo.dom.createDocumentFromText=function(str,_121){
if(!_121){
_121="text/xml";
}
if(!dj_undef("DOMParser")){
var _122=new DOMParser();
return _122.parseFromString(str,_121);
}else{
if(!dj_undef("ActiveXObject")){
var _123=dojo.dom.createDocument();
if(_123){
_123.async=false;
_123.loadXML(str);
return _123;
}else{
dojo.debug("toXml didn't work?");
}
}else{
var _124=dojo.doc();
if(_124.createElement){
var tmp=_124.createElement("xml");
tmp.innerHTML=str;
if(_124.implementation&&_124.implementation.createDocument){
var _126=_124.implementation.createDocument("foo","",null);
for(var i=0;i<tmp.childNodes.length;i++){
_126.importNode(tmp.childNodes.item(i),true);
}
return _126;
}
return ((tmp.document)&&(tmp.document.firstChild?tmp.document.firstChild:tmp));
}
}
}
return null;
};
dojo.dom.prependChild=function(node,_129){
if(_129.firstChild){
_129.insertBefore(node,_129.firstChild);
}else{
_129.appendChild(node);
}
return true;
};
dojo.dom.insertBefore=function(node,ref,_12c){
if((_12c!=true)&&(node===ref||node.nextSibling===ref)){
return false;
}
var _12d=ref.parentNode;
_12d.insertBefore(node,ref);
return true;
};
dojo.dom.insertAfter=function(node,ref,_130){
var pn=ref.parentNode;
if(ref==pn.lastChild){
if((_130!=true)&&(node===ref)){
return false;
}
pn.appendChild(node);
}else{
return this.insertBefore(node,ref.nextSibling,_130);
}
return true;
};
dojo.dom.insertAtPosition=function(node,ref,_134){
if((!node)||(!ref)||(!_134)){
return false;
}
switch(_134.toLowerCase()){
case "before":
return dojo.dom.insertBefore(node,ref);
case "after":
return dojo.dom.insertAfter(node,ref);
case "first":
if(ref.firstChild){
return dojo.dom.insertBefore(node,ref.firstChild);
}else{
ref.appendChild(node);
return true;
}
break;
default:
ref.appendChild(node);
return true;
}
};
dojo.dom.insertAtIndex=function(node,_136,_137){
var _138=_136.childNodes;
if(!_138.length||_138.length==_137){
_136.appendChild(node);
return true;
}
if(_137==0){
return dojo.dom.prependChild(node,_136);
}
return dojo.dom.insertAfter(node,_138[_137-1]);
};
dojo.dom.textContent=function(node,text){
if(arguments.length>1){
var _13b=dojo.doc();
dojo.dom.replaceChildren(node,_13b.createTextNode(text));
return text;
}else{
if(node.textContent!=undefined){
return node.textContent;
}
var _13c="";
if(node==null){
return _13c;
}
for(var i=0;i<node.childNodes.length;i++){
switch(node.childNodes[i].nodeType){
case 1:
case 5:
_13c+=dojo.dom.textContent(node.childNodes[i]);
break;
case 3:
case 2:
case 4:
_13c+=node.childNodes[i].nodeValue;
break;
default:
break;
}
}
return _13c;
}
};
dojo.dom.hasParent=function(node){
return Boolean(node&&node.parentNode&&dojo.dom.isNode(node.parentNode));
};
dojo.dom.isTag=function(node){
if(node&&node.tagName){
for(var i=1;i<arguments.length;i++){
if(node.tagName==String(arguments[i])){
return String(arguments[i]);
}
}
}
return "";
};
dojo.dom.setAttributeNS=function(elem,_142,_143,_144){
if(elem==null||((elem==undefined)&&(typeof elem=="undefined"))){
dojo.raise("No element given to dojo.dom.setAttributeNS");
}
if(!((elem.setAttributeNS==undefined)&&(typeof elem.setAttributeNS=="undefined"))){
elem.setAttributeNS(_142,_143,_144);
}else{
var _145=elem.ownerDocument;
var _146=_145.createNode(2,_143,_142);
_146.nodeValue=_144;
elem.setAttributeNode(_146);
}
};
dojo.provide("dojo.xml.Parse");
dojo.xml.Parse=function(){
var isIE=((dojo.render.html.capable)&&(dojo.render.html.ie));
function getTagName(node){
try{
return node.tagName.toLowerCase();
}
catch(e){
return "";
}
}
function getDojoTagName(node){
var _14a=getTagName(node);
if(!_14a){
return "";
}
if((dojo.widget)&&(dojo.widget.tags[_14a])){
return _14a;
}
var p=_14a.indexOf(":");
if(p>=0){
return _14a;
}
if(_14a.substr(0,5)=="dojo:"){
return _14a;
}
if(dojo.render.html.capable&&dojo.render.html.ie&&node.scopeName!="HTML"){
return node.scopeName.toLowerCase()+":"+_14a;
}
if(_14a.substr(0,4)=="dojo"){
return "dojo:"+_14a.substring(4);
}
var djt=node.getAttribute("dojoType")||node.getAttribute("dojotype");
if(djt){
if(djt.indexOf(":")<0){
djt="dojo:"+djt;
}
return djt.toLowerCase();
}
djt=node.getAttributeNS&&node.getAttributeNS(dojo.dom.dojoml,"type");
if(djt){
return "dojo:"+djt.toLowerCase();
}
try{
djt=node.getAttribute("dojo:type");
}
catch(e){
}
if(djt){
return "dojo:"+djt.toLowerCase();
}
if((dj_global["djConfig"])&&(!djConfig["ignoreClassNames"])){
var _14d=node.className||node.getAttribute("class");
if((_14d)&&(_14d.indexOf)&&(_14d.indexOf("dojo-")!=-1)){
var _14e=_14d.split(" ");
for(var x=0,c=_14e.length;x<c;x++){
if(_14e[x].slice(0,5)=="dojo-"){
return "dojo:"+_14e[x].substr(5).toLowerCase();
}
}
}
}
return "";
}
this.parseElement=function(node,_152,_153,_154){
var _155=getTagName(node);
if(isIE&&_155.indexOf("/")==0){
return null;
}
try{
var attr=node.getAttribute("parseWidgets");
if(attr&&attr.toLowerCase()=="false"){
return {};
}
}
catch(e){
}
var _157=true;
if(_153){
var _158=getDojoTagName(node);
_155=_158||_155;
_157=Boolean(_158);
}
var _159={};
_159[_155]=[];
var pos=_155.indexOf(":");
if(pos>0){
var ns=_155.substring(0,pos);
_159["ns"]=ns;
if((dojo.ns)&&(!dojo.ns.allow(ns))){
_157=false;
}
}
if(_157){
var _15c=this.parseAttributes(node);
for(var attr in _15c){
if((!_159[_155][attr])||(typeof _159[_155][attr]!="array")){
_159[_155][attr]=[];
}
_159[_155][attr].push(_15c[attr]);
}
_159[_155].nodeRef=node;
_159.tagName=_155;
_159.index=_154||0;
}
var _15d=0;
for(var i=0;i<node.childNodes.length;i++){
var tcn=node.childNodes.item(i);
switch(tcn.nodeType){
case dojo.dom.ELEMENT_NODE:
var ctn=getDojoTagName(tcn)||getTagName(tcn);
if(!_159[ctn]){
_159[ctn]=[];
}
_159[ctn].push(this.parseElement(tcn,true,_153,_15d));
if((tcn.childNodes.length==1)&&(tcn.childNodes.item(0).nodeType==dojo.dom.TEXT_NODE)){
_159[ctn][_159[ctn].length-1].value=tcn.childNodes.item(0).nodeValue;
}
_15d++;
break;
case dojo.dom.TEXT_NODE:
if(node.childNodes.length==1){
_159[_155].push({value:node.childNodes.item(0).nodeValue});
}
break;
default:
break;
}
}
return _159;
};
this.parseAttributes=function(node){
var _162={};
var atts=node.attributes;
var _164,i=0;
while((_164=atts[i++])){
if(isIE){
if(!_164){
continue;
}
if((typeof _164=="object")&&(typeof _164.nodeValue=="undefined")||(_164.nodeValue==null)||(_164.nodeValue=="")){
continue;
}
}
var nn=_164.nodeName.split(":");
nn=(nn.length==2)?nn[1]:_164.nodeName;
_162[nn]={value:_164.nodeValue};
}
return _162;
};
};
dojo.provide("dojo.lang.common");
dojo.lang.inherits=function(_167,_168){
if(!dojo.lang.isFunction(_168)){
dojo.raise("dojo.inherits: superclass argument ["+_168+"] must be a function (subclass: ["+_167+"']");
}
_167.prototype=new _168();
_167.prototype.constructor=_167;
_167.superclass=_168.prototype;
_167["super"]=_168.prototype;
};
dojo.lang._mixin=function(obj,_16a){
var tobj={};
for(var x in _16a){
if((typeof tobj[x]=="undefined")||(tobj[x]!=_16a[x])){
obj[x]=_16a[x];
}
}
if(dojo.render.html.ie&&(typeof (_16a["toString"])=="function")&&(_16a["toString"]!=obj["toString"])&&(_16a["toString"]!=tobj["toString"])){
obj.toString=_16a.toString;
}
return obj;
};
dojo.lang.mixin=function(obj,_16e){
for(var i=1,l=arguments.length;i<l;i++){
dojo.lang._mixin(obj,arguments[i]);
}
return obj;
};
dojo.lang.extend=function(_171,_172){
for(var i=1,l=arguments.length;i<l;i++){
dojo.lang._mixin(_171.prototype,arguments[i]);
}
return _171;
};
dojo.inherits=dojo.lang.inherits;
dojo.mixin=dojo.lang.mixin;
dojo.extend=dojo.lang.extend;
dojo.lang.find=function(_175,_176,_177,_178){
if(!dojo.lang.isArrayLike(_175)&&dojo.lang.isArrayLike(_176)){
dojo.deprecated("dojo.lang.find(value, array)","use dojo.lang.find(array, value) instead","0.5");
var temp=_175;
_175=_176;
_176=temp;
}
var _17a=dojo.lang.isString(_175);
if(_17a){
_175=_175.split("");
}
if(_178){
var step=-1;
var i=_175.length-1;
var end=-1;
}else{
var step=1;
var i=0;
var end=_175.length;
}
if(_177){
while(i!=end){
if(_175[i]===_176){
return i;
}
i+=step;
}
}else{
while(i!=end){
if(_175[i]==_176){
return i;
}
i+=step;
}
}
return -1;
};
dojo.lang.indexOf=dojo.lang.find;
dojo.lang.findLast=function(_17e,_17f,_180){
return dojo.lang.find(_17e,_17f,_180,true);
};
dojo.lang.lastIndexOf=dojo.lang.findLast;
dojo.lang.inArray=function(_181,_182){
return dojo.lang.find(_181,_182)>-1;
};
dojo.lang.isObject=function(it){
if(typeof it=="undefined"){
return false;
}
return (typeof it=="object"||it===null||dojo.lang.isArray(it)||dojo.lang.isFunction(it));
};
dojo.lang.isArray=function(it){
return (it&&it instanceof Array||typeof it=="array");
};
dojo.lang.isArrayLike=function(it){
if((!it)||(dojo.lang.isUndefined(it))){
return false;
}
if(dojo.lang.isString(it)){
return false;
}
if(dojo.lang.isFunction(it)){
return false;
}
if(dojo.lang.isArray(it)){
return true;
}
if((it.tagName)&&(it.tagName.toLowerCase()=="form")){
return false;
}
if(dojo.lang.isNumber(it.length)&&isFinite(it.length)){
return true;
}
return false;
};
dojo.lang.isFunction=function(it){
return (it instanceof Function||typeof it=="function");
};
(function(){
if((dojo.render.html.capable)&&(dojo.render.html["safari"])){
dojo.lang.isFunction=function(it){
if((typeof (it)=="function")&&(it=="[object NodeList]")){
return false;
}
return (it instanceof Function||typeof it=="function");
};
}
})();
dojo.lang.isString=function(it){
return (typeof it=="string"||it instanceof String);
};
dojo.lang.isAlien=function(it){
if(!it){
return false;
}
return !dojo.lang.isFunction(it)&&/\{\s*\[native code\]\s*\}/.test(String(it));
};
dojo.lang.isBoolean=function(it){
return (it instanceof Boolean||typeof it=="boolean");
};
dojo.lang.isNumber=function(it){
return (it instanceof Number||typeof it=="number");
};
dojo.lang.isUndefined=function(it){
return ((typeof (it)=="undefined")&&(it==undefined));
};
dojo.provide("dojo.lang.func");
dojo.lang.hitch=function(_18d,_18e){
var fcn=(dojo.lang.isString(_18e)?_18d[_18e]:_18e)||function(){
};
return function(){
return fcn.apply(_18d,arguments);
};
};
dojo.lang.anonCtr=0;
dojo.lang.anon={};
dojo.lang.nameAnonFunc=function(_190,_191,_192){
var nso=(_191||dojo.lang.anon);
if((_192)||((dj_global["djConfig"])&&(djConfig["slowAnonFuncLookups"]==true))){
for(var x in nso){
try{
if(nso[x]===_190){
return x;
}
}
catch(e){
}
}
}
var ret="__"+dojo.lang.anonCtr++;
while(typeof nso[ret]!="undefined"){
ret="__"+dojo.lang.anonCtr++;
}
nso[ret]=_190;
return ret;
};
dojo.lang.forward=function(_196){
return function(){
return this[_196].apply(this,arguments);
};
};
dojo.lang.curry=function(_197,func){
var _199=[];
_197=_197||dj_global;
if(dojo.lang.isString(func)){
func=_197[func];
}
for(var x=2;x<arguments.length;x++){
_199.push(arguments[x]);
}
var _19b=(func["__preJoinArity"]||func.length)-_199.length;
function gather(_19c,_19d,_19e){
var _19f=_19e;
var _1a0=_19d.slice(0);
for(var x=0;x<_19c.length;x++){
_1a0.push(_19c[x]);
}
_19e=_19e-_19c.length;
if(_19e<=0){
var res=func.apply(_197,_1a0);
_19e=_19f;
return res;
}else{
return function(){
return gather(arguments,_1a0,_19e);
};
}
}
return gather([],_199,_19b);
};
dojo.lang.curryArguments=function(_1a3,func,args,_1a6){
var _1a7=[];
var x=_1a6||0;
for(x=_1a6;x<args.length;x++){
_1a7.push(args[x]);
}
return dojo.lang.curry.apply(dojo.lang,[_1a3,func].concat(_1a7));
};
dojo.lang.tryThese=function(){
for(var x=0;x<arguments.length;x++){
try{
if(typeof arguments[x]=="function"){
var ret=(arguments[x]());
if(ret){
return ret;
}
}
}
catch(e){
dojo.debug(e);
}
}
};
dojo.lang.delayThese=function(farr,cb,_1ad,_1ae){
if(!farr.length){
if(typeof _1ae=="function"){
_1ae();
}
return;
}
if((typeof _1ad=="undefined")&&(typeof cb=="number")){
_1ad=cb;
cb=function(){
};
}else{
if(!cb){
cb=function(){
};
if(!_1ad){
_1ad=0;
}
}
}
setTimeout(function(){
(farr.shift())();
cb();
dojo.lang.delayThese(farr,cb,_1ad,_1ae);
},_1ad);
};
dojo.provide("dojo.lang.array");
dojo.lang.mixin(dojo.lang,{has:function(obj,name){
try{
return typeof obj[name]!="undefined";
}
catch(e){
return false;
}
},isEmpty:function(obj){
if(dojo.lang.isObject(obj)){
var tmp={};
var _1b3=0;
for(var x in obj){
if(obj[x]&&(!tmp[x])){
_1b3++;
break;
}
}
return _1b3==0;
}else{
if(dojo.lang.isArrayLike(obj)||dojo.lang.isString(obj)){
return obj.length==0;
}
}
},map:function(arr,obj,_1b7){
var _1b8=dojo.lang.isString(arr);
if(_1b8){
arr=arr.split("");
}
if(dojo.lang.isFunction(obj)&&(!_1b7)){
_1b7=obj;
obj=dj_global;
}else{
if(dojo.lang.isFunction(obj)&&_1b7){
var _1b9=obj;
obj=_1b7;
_1b7=_1b9;
}
}
if(Array.map){
var _1ba=Array.map(arr,_1b7,obj);
}else{
var _1ba=[];
for(var i=0;i<arr.length;++i){
_1ba.push(_1b7.call(obj,arr[i]));
}
}
if(_1b8){
return _1ba.join("");
}else{
return _1ba;
}
},reduce:function(arr,_1bd,obj,_1bf){
var _1c0=_1bd;
if(arguments.length==1){
dojo.debug("dojo.lang.reduce called with too few arguments!");
return false;
}else{
if(arguments.length==2){
_1bf=_1bd;
_1c0=arr.shift();
}else{
if(arguments.lenght==3){
if(dojo.lang.isFunction(obj)){
_1bf=obj;
obj=null;
}
}else{
if(dojo.lang.isFunction(obj)){
var tmp=_1bf;
_1bf=obj;
obj=tmp;
}
}
}
}
var ob=obj?obj:dj_global;
dojo.lang.map(arr,function(val){
_1c0=_1bf.call(ob,_1c0,val);
});
return _1c0;
},forEach:function(_1c4,_1c5,_1c6){
if(dojo.lang.isString(_1c4)){
_1c4=_1c4.split("");
}
if(Array.forEach){
Array.forEach(_1c4,_1c5,_1c6);
}else{
if(!_1c6){
_1c6=dj_global;
}
for(var i=0,l=_1c4.length;i<l;i++){
_1c5.call(_1c6,_1c4[i],i,_1c4);
}
}
},_everyOrSome:function(_1c9,arr,_1cb,_1cc){
if(dojo.lang.isString(arr)){
arr=arr.split("");
}
if(Array.every){
return Array[_1c9?"every":"some"](arr,_1cb,_1cc);
}else{
if(!_1cc){
_1cc=dj_global;
}
for(var i=0,l=arr.length;i<l;i++){
var _1cf=_1cb.call(_1cc,arr[i],i,arr);
if(_1c9&&!_1cf){
return false;
}else{
if((!_1c9)&&(_1cf)){
return true;
}
}
}
return Boolean(_1c9);
}
},every:function(arr,_1d1,_1d2){
return this._everyOrSome(true,arr,_1d1,_1d2);
},some:function(arr,_1d4,_1d5){
return this._everyOrSome(false,arr,_1d4,_1d5);
},filter:function(arr,_1d7,_1d8){
var _1d9=dojo.lang.isString(arr);
if(_1d9){
arr=arr.split("");
}
var _1da;
if(Array.filter){
_1da=Array.filter(arr,_1d7,_1d8);
}else{
if(!_1d8){
if(arguments.length>=3){
dojo.raise("thisObject doesn't exist!");
}
_1d8=dj_global;
}
_1da=[];
for(var i=0;i<arr.length;i++){
if(_1d7.call(_1d8,arr[i],i,arr)){
_1da.push(arr[i]);
}
}
}
if(_1d9){
return _1da.join("");
}else{
return _1da;
}
},unnest:function(){
var out=[];
for(var i=0;i<arguments.length;i++){
if(dojo.lang.isArrayLike(arguments[i])){
var add=dojo.lang.unnest.apply(this,arguments[i]);
out=out.concat(add);
}else{
out.push(arguments[i]);
}
}
return out;
},toArray:function(_1df,_1e0){
var _1e1=[];
for(var i=_1e0||0;i<_1df.length;i++){
_1e1.push(_1df[i]);
}
return _1e1;
}});
dojo.provide("dojo.lang.extras");
dojo.lang.setTimeout=function(func,_1e4){
var _1e5=window,_1e6=2;
if(!dojo.lang.isFunction(func)){
_1e5=func;
func=_1e4;
_1e4=arguments[2];
_1e6++;
}
if(dojo.lang.isString(func)){
func=_1e5[func];
}
var args=[];
for(var i=_1e6;i<arguments.length;i++){
args.push(arguments[i]);
}
return dojo.global().setTimeout(function(){
func.apply(_1e5,args);
},_1e4);
};
dojo.lang.clearTimeout=function(_1e9){
dojo.global().clearTimeout(_1e9);
};
dojo.lang.getNameInObj=function(ns,item){
if(!ns){
ns=dj_global;
}
for(var x in ns){
if(ns[x]===item){
return new String(x);
}
}
return null;
};
dojo.lang.shallowCopy=function(obj,deep){
var i,ret;
if(obj===null){
return null;
}
if(dojo.lang.isObject(obj)){
ret=new obj.constructor();
for(i in obj){
if(dojo.lang.isUndefined(ret[i])){
ret[i]=deep?dojo.lang.shallowCopy(obj[i],deep):obj[i];
}
}
}else{
if(dojo.lang.isArray(obj)){
ret=[];
for(i=0;i<obj.length;i++){
ret[i]=deep?dojo.lang.shallowCopy(obj[i],deep):obj[i];
}
}else{
ret=obj;
}
}
return ret;
};
dojo.lang.firstValued=function(){
for(var i=0;i<arguments.length;i++){
if(typeof arguments[i]!="undefined"){
return arguments[i];
}
}
return undefined;
};
dojo.lang.getObjPathValue=function(_1f2,_1f3,_1f4){
with(dojo.parseObjPath(_1f2,_1f3,_1f4)){
return dojo.evalProp(prop,obj,_1f4);
}
};
dojo.lang.setObjPathValue=function(_1f5,_1f6,_1f7,_1f8){
dojo.deprecated("dojo.lang.setObjPathValue","use dojo.parseObjPath and the '=' operator","0.6");
if(arguments.length<4){
_1f8=true;
}
with(dojo.parseObjPath(_1f5,_1f7,_1f8)){
if(obj&&(_1f8||(prop in obj))){
obj[prop]=_1f6;
}
}
};
dojo.provide("dojo.lang.declare");
dojo.lang.declare=function(_1f9,_1fa,init,_1fc){
if((dojo.lang.isFunction(_1fc))||((!_1fc)&&(!dojo.lang.isFunction(init)))){
var temp=_1fc;
_1fc=init;
init=temp;
}
var _1fe=[];
if(dojo.lang.isArray(_1fa)){
_1fe=_1fa;
_1fa=_1fe.shift();
}
if(!init){
init=dojo.evalObjPath(_1f9,false);
if((init)&&(!dojo.lang.isFunction(init))){
init=null;
}
}
var ctor=dojo.lang.declare._makeConstructor();
var scp=(_1fa?_1fa.prototype:null);
if(scp){
scp.prototyping=true;
ctor.prototype=new _1fa();
scp.prototyping=false;
}
ctor.superclass=scp;
ctor.mixins=_1fe;
for(var i=0,l=_1fe.length;i<l;i++){
dojo.lang.extend(ctor,_1fe[i].prototype);
}
ctor.prototype.initializer=null;
ctor.prototype.declaredClass=_1f9;
if(dojo.lang.isArray(_1fc)){
dojo.lang.extend.apply(dojo.lang,[ctor].concat(_1fc));
}else{
dojo.lang.extend(ctor,(_1fc)||{});
}
dojo.lang.extend(ctor,dojo.lang.declare._common);
ctor.prototype.constructor=ctor;
ctor.prototype.initializer=(ctor.prototype.initializer)||(init)||(function(){
});
var _203=dojo.parseObjPath(_1f9,null,true);
_203.obj[_203.prop]=ctor;
return ctor;
};
dojo.lang.declare._makeConstructor=function(){
return function(){
var self=this._getPropContext();
var s=self.constructor.superclass;
if((s)&&(s.constructor)){
if(s.constructor==arguments.callee){
this._inherited("constructor",arguments);
}else{
this._contextMethod(s,"constructor",arguments);
}
}
var ms=(self.constructor.mixins)||([]);
for(var i=0,m;(m=ms[i]);i++){
(((m.prototype)&&(m.prototype.initializer))||(m)).apply(this,arguments);
}
if((!this.prototyping)&&(self.initializer)){
self.initializer.apply(this,arguments);
}
};
};
dojo.lang.declare._common={_getPropContext:function(){
return (this.___proto||this);
},_contextMethod:function(_209,_20a,args){
var _20c,_20d=this.___proto;
this.___proto=_209;
try{
_20c=_209[_20a].apply(this,(args||[]));
}
catch(e){
throw e;
}
finally{
this.___proto=_20d;
}
return _20c;
},_inherited:function(prop,args){
var p=this._getPropContext();
do{
if((!p.constructor)||(!p.constructor.superclass)){
return;
}
p=p.constructor.superclass;
}while(!(prop in p));
return (dojo.lang.isFunction(p[prop])?this._contextMethod(p,prop,args):p[prop]);
},inherited:function(prop,args){
dojo.deprecated("'inherited' method is dangerous, do not up-call! 'inherited' is slated for removal in 0.5; name your super class (or use superclass property) instead.","0.5");
this._inherited(prop,args);
}};
dojo.declare=dojo.lang.declare;
dojo.provide("dojo.ns");
dojo.ns={namespaces:{},failed:{},loading:{},loaded:{},register:function(name,_214,_215,_216){
if(!_216||!this.namespaces[name]){
this.namespaces[name]=new dojo.ns.Ns(name,_214,_215);
}
},allow:function(name){
if(this.failed[name]){
return false;
}
if((djConfig.excludeNamespace)&&(dojo.lang.inArray(djConfig.excludeNamespace,name))){
return false;
}
return ((name==this.dojo)||(!djConfig.includeNamespace)||(dojo.lang.inArray(djConfig.includeNamespace,name)));
},get:function(name){
return this.namespaces[name];
},require:function(name){
var ns=this.namespaces[name];
if((ns)&&(this.loaded[name])){
return ns;
}
if(!this.allow(name)){
return false;
}
if(this.loading[name]){
dojo.debug("dojo.namespace.require: re-entrant request to load namespace \""+name+"\" must fail.");
return false;
}
var req=dojo.require;
this.loading[name]=true;
try{
if(name=="dojo"){
req("dojo.namespaces.dojo");
}else{
if(!dojo.hostenv.moduleHasPrefix(name)){
dojo.registerModulePath(name,"../"+name);
}
req([name,"manifest"].join("."),false,true);
}
if(!this.namespaces[name]){
this.failed[name]=true;
}
}
finally{
this.loading[name]=false;
}
return this.namespaces[name];
}};
dojo.ns.Ns=function(name,_21d,_21e){
this.name=name;
this.module=_21d;
this.resolver=_21e;
this._loaded=[];
this._failed=[];
};
dojo.ns.Ns.prototype.resolve=function(name,_220,_221){
if(!this.resolver||djConfig["skipAutoRequire"]){
return false;
}
var _222=this.resolver(name,_220);
if((_222)&&(!this._loaded[_222])&&(!this._failed[_222])){
var req=dojo.require;
req(_222,false,true);
if(dojo.hostenv.findModule(_222,false)){
this._loaded[_222]=true;
}else{
if(!_221){
dojo.raise("dojo.ns.Ns.resolve: module '"+_222+"' not found after loading via namespace '"+this.name+"'");
}
this._failed[_222]=true;
}
}
return Boolean(this._loaded[_222]);
};
dojo.registerNamespace=function(name,_225,_226){
dojo.ns.register.apply(dojo.ns,arguments);
};
dojo.registerNamespaceResolver=function(name,_228){
var n=dojo.ns.namespaces[name];
if(n){
n.resolver=_228;
}
};
dojo.registerNamespaceManifest=function(_22a,path,name,_22d,_22e){
dojo.registerModulePath(name,path);
dojo.registerNamespace(name,_22d,_22e);
};
dojo.registerNamespace("dojo","dojo.widget");
dojo.provide("dojo.event.common");
dojo.event=new function(){
this._canTimeout=dojo.lang.isFunction(dj_global["setTimeout"])||dojo.lang.isAlien(dj_global["setTimeout"]);
function interpolateArgs(args,_230){
var dl=dojo.lang;
var ao={srcObj:dj_global,srcFunc:null,adviceObj:dj_global,adviceFunc:null,aroundObj:null,aroundFunc:null,adviceType:(args.length>2)?args[0]:"after",precedence:"last",once:false,delay:null,rate:0,adviceMsg:false};
switch(args.length){
case 0:
return;
case 1:
return;
case 2:
ao.srcFunc=args[0];
ao.adviceFunc=args[1];
break;
case 3:
if((dl.isObject(args[0]))&&(dl.isString(args[1]))&&(dl.isString(args[2]))){
ao.adviceType="after";
ao.srcObj=args[0];
ao.srcFunc=args[1];
ao.adviceFunc=args[2];
}else{
if((dl.isString(args[1]))&&(dl.isString(args[2]))){
ao.srcFunc=args[1];
ao.adviceFunc=args[2];
}else{
if((dl.isObject(args[0]))&&(dl.isString(args[1]))&&(dl.isFunction(args[2]))){
ao.adviceType="after";
ao.srcObj=args[0];
ao.srcFunc=args[1];
var _233=dl.nameAnonFunc(args[2],ao.adviceObj,_230);
ao.adviceFunc=_233;
}else{
if((dl.isFunction(args[0]))&&(dl.isObject(args[1]))&&(dl.isString(args[2]))){
ao.adviceType="after";
ao.srcObj=dj_global;
var _233=dl.nameAnonFunc(args[0],ao.srcObj,_230);
ao.srcFunc=_233;
ao.adviceObj=args[1];
ao.adviceFunc=args[2];
}
}
}
}
break;
case 4:
if((dl.isObject(args[0]))&&(dl.isObject(args[2]))){
ao.adviceType="after";
ao.srcObj=args[0];
ao.srcFunc=args[1];
ao.adviceObj=args[2];
ao.adviceFunc=args[3];
}else{
if((dl.isString(args[0]))&&(dl.isString(args[1]))&&(dl.isObject(args[2]))){
ao.adviceType=args[0];
ao.srcObj=dj_global;
ao.srcFunc=args[1];
ao.adviceObj=args[2];
ao.adviceFunc=args[3];
}else{
if((dl.isString(args[0]))&&(dl.isFunction(args[1]))&&(dl.isObject(args[2]))){
ao.adviceType=args[0];
ao.srcObj=dj_global;
var _233=dl.nameAnonFunc(args[1],dj_global,_230);
ao.srcFunc=_233;
ao.adviceObj=args[2];
ao.adviceFunc=args[3];
}else{
if((dl.isString(args[0]))&&(dl.isObject(args[1]))&&(dl.isString(args[2]))&&(dl.isFunction(args[3]))){
ao.srcObj=args[1];
ao.srcFunc=args[2];
var _233=dl.nameAnonFunc(args[3],dj_global,_230);
ao.adviceObj=dj_global;
ao.adviceFunc=_233;
}else{
if(dl.isObject(args[1])){
ao.srcObj=args[1];
ao.srcFunc=args[2];
ao.adviceObj=dj_global;
ao.adviceFunc=args[3];
}else{
if(dl.isObject(args[2])){
ao.srcObj=dj_global;
ao.srcFunc=args[1];
ao.adviceObj=args[2];
ao.adviceFunc=args[3];
}else{
ao.srcObj=ao.adviceObj=ao.aroundObj=dj_global;
ao.srcFunc=args[1];
ao.adviceFunc=args[2];
ao.aroundFunc=args[3];
}
}
}
}
}
}
break;
case 6:
ao.srcObj=args[1];
ao.srcFunc=args[2];
ao.adviceObj=args[3];
ao.adviceFunc=args[4];
ao.aroundFunc=args[5];
ao.aroundObj=dj_global;
break;
default:
ao.srcObj=args[1];
ao.srcFunc=args[2];
ao.adviceObj=args[3];
ao.adviceFunc=args[4];
ao.aroundObj=args[5];
ao.aroundFunc=args[6];
ao.once=args[7];
ao.delay=args[8];
ao.rate=args[9];
ao.adviceMsg=args[10];
break;
}
if(dl.isFunction(ao.aroundFunc)){
var _233=dl.nameAnonFunc(ao.aroundFunc,ao.aroundObj,_230);
ao.aroundFunc=_233;
}
if(dl.isFunction(ao.srcFunc)){
ao.srcFunc=dl.getNameInObj(ao.srcObj,ao.srcFunc);
}
if(dl.isFunction(ao.adviceFunc)){
ao.adviceFunc=dl.getNameInObj(ao.adviceObj,ao.adviceFunc);
}
if((ao.aroundObj)&&(dl.isFunction(ao.aroundFunc))){
ao.aroundFunc=dl.getNameInObj(ao.aroundObj,ao.aroundFunc);
}
if(!ao.srcObj){
dojo.raise("bad srcObj for srcFunc: "+ao.srcFunc);
}
if(!ao.adviceObj){
dojo.raise("bad adviceObj for adviceFunc: "+ao.adviceFunc);
}
if(!ao.adviceFunc){
dojo.debug("bad adviceFunc for srcFunc: "+ao.srcFunc);
dojo.debugShallow(ao);
}
return ao;
}
this.connect=function(){
if(arguments.length==1){
var ao=arguments[0];
}else{
var ao=interpolateArgs(arguments,true);
}
if(dojo.lang.isString(ao.srcFunc)&&(ao.srcFunc.toLowerCase()=="onkey")){
if(dojo.render.html.ie){
ao.srcFunc="onkeydown";
this.connect(ao);
}
ao.srcFunc="onkeypress";
}
if(dojo.lang.isArray(ao.srcObj)&&ao.srcObj!=""){
var _235={};
for(var x in ao){
_235[x]=ao[x];
}
var mjps=[];
dojo.lang.forEach(ao.srcObj,function(src){
if((dojo.render.html.capable)&&(dojo.lang.isString(src))){
src=dojo.byId(src);
}
_235.srcObj=src;
mjps.push(dojo.event.connect.call(dojo.event,_235));
});
return mjps;
}
var mjp=dojo.event.MethodJoinPoint.getForMethod(ao.srcObj,ao.srcFunc);
if(ao.adviceFunc){
var mjp2=dojo.event.MethodJoinPoint.getForMethod(ao.adviceObj,ao.adviceFunc);
}
mjp.kwAddAdvice(ao);
return mjp;
};
this.log=function(a1,a2){
var _23d;
if((arguments.length==1)&&(typeof a1=="object")){
_23d=a1;
}else{
_23d={srcObj:a1,srcFunc:a2};
}
_23d.adviceFunc=function(){
var _23e=[];
for(var x=0;x<arguments.length;x++){
_23e.push(arguments[x]);
}
dojo.debug("("+_23d.srcObj+")."+_23d.srcFunc,":",_23e.join(", "));
};
this.kwConnect(_23d);
};
this.connectBefore=function(){
var args=["before"];
for(var i=0;i<arguments.length;i++){
args.push(arguments[i]);
}
return this.connect.apply(this,args);
};
this.connectAround=function(){
var args=["around"];
for(var i=0;i<arguments.length;i++){
args.push(arguments[i]);
}
return this.connect.apply(this,args);
};
this.connectOnce=function(){
var ao=interpolateArgs(arguments,true);
ao.once=true;
return this.connect(ao);
};
this._kwConnectImpl=function(_245,_246){
var fn=(_246)?"disconnect":"connect";
if(typeof _245["srcFunc"]=="function"){
_245.srcObj=_245["srcObj"]||dj_global;
var _248=dojo.lang.nameAnonFunc(_245.srcFunc,_245.srcObj,true);
_245.srcFunc=_248;
}
if(typeof _245["adviceFunc"]=="function"){
_245.adviceObj=_245["adviceObj"]||dj_global;
var _248=dojo.lang.nameAnonFunc(_245.adviceFunc,_245.adviceObj,true);
_245.adviceFunc=_248;
}
_245.srcObj=_245["srcObj"]||dj_global;
_245.adviceObj=_245["adviceObj"]||_245["targetObj"]||dj_global;
_245.adviceFunc=_245["adviceFunc"]||_245["targetFunc"];
return dojo.event[fn](_245);
};
this.kwConnect=function(_249){
return this._kwConnectImpl(_249,false);
};
this.disconnect=function(){
if(arguments.length==1){
var ao=arguments[0];
}else{
var ao=interpolateArgs(arguments,true);
}
if(!ao.adviceFunc){
return;
}
if(dojo.lang.isString(ao.srcFunc)&&(ao.srcFunc.toLowerCase()=="onkey")){
if(dojo.render.html.ie){
ao.srcFunc="onkeydown";
this.disconnect(ao);
}
ao.srcFunc="onkeypress";
}
if(!ao.srcObj[ao.srcFunc]){
return null;
}
var mjp=dojo.event.MethodJoinPoint.getForMethod(ao.srcObj,ao.srcFunc,true);
mjp.removeAdvice(ao.adviceObj,ao.adviceFunc,ao.adviceType,ao.once);
return mjp;
};
this.kwDisconnect=function(_24c){
return this._kwConnectImpl(_24c,true);
};
};
dojo.event.MethodInvocation=function(_24d,obj,args){
this.jp_=_24d;
this.object=obj;
this.args=[];
for(var x=0;x<args.length;x++){
this.args[x]=args[x];
}
this.around_index=-1;
};
dojo.event.MethodInvocation.prototype.proceed=function(){
this.around_index++;
if(this.around_index>=this.jp_.around.length){
return this.jp_.object[this.jp_.methodname].apply(this.jp_.object,this.args);
}else{
var ti=this.jp_.around[this.around_index];
var mobj=ti[0]||dj_global;
var meth=ti[1];
return mobj[meth].call(mobj,this);
}
};
dojo.event.MethodJoinPoint=function(obj,_255){
this.object=obj||dj_global;
this.methodname=_255;
this.methodfunc=this.object[_255];
this.squelch=false;
};
dojo.event.MethodJoinPoint.getForMethod=function(obj,_257){
if(!obj){
obj=dj_global;
}
if(!obj[_257]){
obj[_257]=function(){
};
if(!obj[_257]){
dojo.raise("Cannot set do-nothing method on that object "+_257);
}
}else{
if((!dojo.lang.isFunction(obj[_257]))&&(!dojo.lang.isAlien(obj[_257]))){
return null;
}
}
var _258=_257+"$joinpoint";
var _259=_257+"$joinpoint$method";
var _25a=obj[_258];
if(!_25a){
var _25b=false;
if(dojo.event["browser"]){
if((obj["attachEvent"])||(obj["nodeType"])||(obj["addEventListener"])){
_25b=true;
dojo.event.browser.addClobberNodeAttrs(obj,[_258,_259,_257]);
}
}
var _25c=obj[_257].length;
obj[_259]=obj[_257];
_25a=obj[_258]=new dojo.event.MethodJoinPoint(obj,_259);
obj[_257]=function(){
var args=[];
if((_25b)&&(!arguments.length)){
var evt=null;
try{
if(obj.ownerDocument){
evt=obj.ownerDocument.parentWindow.event;
}else{
if(obj.documentElement){
evt=obj.documentElement.ownerDocument.parentWindow.event;
}else{
if(obj.event){
evt=obj.event;
}else{
evt=window.event;
}
}
}
}
catch(e){
evt=window.event;
}
if(evt){
args.push(dojo.event.browser.fixEvent(evt,this));
}
}else{
for(var x=0;x<arguments.length;x++){
if((x==0)&&(_25b)&&(dojo.event.browser.isEvent(arguments[x]))){
args.push(dojo.event.browser.fixEvent(arguments[x],this));
}else{
args.push(arguments[x]);
}
}
}
return _25a.run.apply(_25a,args);
};
obj[_257].__preJoinArity=_25c;
}
return _25a;
};
dojo.lang.extend(dojo.event.MethodJoinPoint,{unintercept:function(){
this.object[this.methodname]=this.methodfunc;
this.before=[];
this.after=[];
this.around=[];
},disconnect:dojo.lang.forward("unintercept"),run:function(){
var obj=this.object||dj_global;
var args=arguments;
var _262=[];
for(var x=0;x<args.length;x++){
_262[x]=args[x];
}
var _264=function(marr){
if(!marr){
dojo.debug("Null argument to unrollAdvice()");
return;
}
var _266=marr[0]||dj_global;
var _267=marr[1];
if(!_266[_267]){
dojo.raise("function \""+_267+"\" does not exist on \""+_266+"\"");
}
var _268=marr[2]||dj_global;
var _269=marr[3];
var msg=marr[6];
var _26b;
var to={args:[],jp_:this,object:obj,proceed:function(){
return _266[_267].apply(_266,to.args);
}};
to.args=_262;
var _26d=parseInt(marr[4]);
var _26e=((!isNaN(_26d))&&(marr[4]!==null)&&(typeof marr[4]!="undefined"));
if(marr[5]){
var rate=parseInt(marr[5]);
var cur=new Date();
var _271=false;
if((marr["last"])&&((cur-marr.last)<=rate)){
if(dojo.event._canTimeout){
if(marr["delayTimer"]){
clearTimeout(marr.delayTimer);
}
var tod=parseInt(rate*2);
var mcpy=dojo.lang.shallowCopy(marr);
marr.delayTimer=setTimeout(function(){
mcpy[5]=0;
_264(mcpy);
},tod);
}
return;
}else{
marr.last=cur;
}
}
if(_269){
_268[_269].call(_268,to);
}else{
if((_26e)&&((dojo.render.html)||(dojo.render.svg))){
dj_global["setTimeout"](function(){
if(msg){
_266[_267].call(_266,to);
}else{
_266[_267].apply(_266,args);
}
},_26d);
}else{
if(msg){
_266[_267].call(_266,to);
}else{
_266[_267].apply(_266,args);
}
}
}
};
var _274=function(){
if(this.squelch){
try{
return _264.apply(this,arguments);
}
catch(e){
dojo.debug(e);
}
}else{
return _264.apply(this,arguments);
}
};
if((this["before"])&&(this.before.length>0)){
dojo.lang.forEach(this.before.concat(new Array()),_274);
}
var _275;
try{
if((this["around"])&&(this.around.length>0)){
var mi=new dojo.event.MethodInvocation(this,obj,args);
_275=mi.proceed();
}else{
if(this.methodfunc){
_275=this.object[this.methodname].apply(this.object,args);
}
}
}
catch(e){
if(!this.squelch){
dojo.debug(e,"when calling",this.methodname,"on",this.object,"with arguments",args);
dojo.raise(e);
}
}
if((this["after"])&&(this.after.length>0)){
dojo.lang.forEach(this.after.concat(new Array()),_274);
}
return (this.methodfunc)?_275:null;
},getArr:function(kind){
var type="after";
if((typeof kind=="string")&&(kind.indexOf("before")!=-1)){
type="before";
}else{
if(kind=="around"){
type="around";
}
}
if(!this[type]){
this[type]=[];
}
return this[type];
},kwAddAdvice:function(args){
this.addAdvice(args["adviceObj"],args["adviceFunc"],args["aroundObj"],args["aroundFunc"],args["adviceType"],args["precedence"],args["once"],args["delay"],args["rate"],args["adviceMsg"]);
},addAdvice:function(_27a,_27b,_27c,_27d,_27e,_27f,once,_281,rate,_283){
var arr=this.getArr(_27e);
if(!arr){
dojo.raise("bad this: "+this);
}
var ao=[_27a,_27b,_27c,_27d,_281,rate,_283];
if(once){
if(this.hasAdvice(_27a,_27b,_27e,arr)>=0){
return;
}
}
if(_27f=="first"){
arr.unshift(ao);
}else{
arr.push(ao);
}
},hasAdvice:function(_286,_287,_288,arr){
if(!arr){
arr=this.getArr(_288);
}
var ind=-1;
for(var x=0;x<arr.length;x++){
var aao=(typeof _287=="object")?(new String(_287)).toString():_287;
var a1o=(typeof arr[x][1]=="object")?(new String(arr[x][1])).toString():arr[x][1];
if((arr[x][0]==_286)&&(a1o==aao)){
ind=x;
}
}
return ind;
},removeAdvice:function(_28e,_28f,_290,once){
var arr=this.getArr(_290);
var ind=this.hasAdvice(_28e,_28f,_290,arr);
if(ind==-1){
return false;
}
while(ind!=-1){
arr.splice(ind,1);
if(once){
break;
}
ind=this.hasAdvice(_28e,_28f,_290,arr);
}
return true;
}});
dojo.provide("dojo.event.topic");
dojo.event.topic=new function(){
this.topics={};
this.getTopic=function(_294){
if(!this.topics[_294]){
this.topics[_294]=new this.TopicImpl(_294);
}
return this.topics[_294];
};
this.registerPublisher=function(_295,obj,_297){
var _295=this.getTopic(_295);
_295.registerPublisher(obj,_297);
};
this.subscribe=function(_298,obj,_29a){
var _298=this.getTopic(_298);
_298.subscribe(obj,_29a);
};
this.unsubscribe=function(_29b,obj,_29d){
var _29b=this.getTopic(_29b);
_29b.unsubscribe(obj,_29d);
};
this.destroy=function(_29e){
this.getTopic(_29e).destroy();
delete this.topics[_29e];
};
this.publishApply=function(_29f,args){
var _29f=this.getTopic(_29f);
_29f.sendMessage.apply(_29f,args);
};
this.publish=function(_2a1,_2a2){
var _2a1=this.getTopic(_2a1);
var args=[];
for(var x=1;x<arguments.length;x++){
args.push(arguments[x]);
}
_2a1.sendMessage.apply(_2a1,args);
};
};
dojo.event.topic.TopicImpl=function(_2a5){
this.topicName=_2a5;
this.subscribe=function(_2a6,_2a7){
var tf=_2a7||_2a6;
var to=(!_2a7)?dj_global:_2a6;
return dojo.event.kwConnect({srcObj:this,srcFunc:"sendMessage",adviceObj:to,adviceFunc:tf});
};
this.unsubscribe=function(_2aa,_2ab){
var tf=(!_2ab)?_2aa:_2ab;
var to=(!_2ab)?null:_2aa;
return dojo.event.kwDisconnect({srcObj:this,srcFunc:"sendMessage",adviceObj:to,adviceFunc:tf});
};
this._getJoinPoint=function(){
return dojo.event.MethodJoinPoint.getForMethod(this,"sendMessage");
};
this.setSquelch=function(_2ae){
this._getJoinPoint().squelch=_2ae;
};
this.destroy=function(){
this._getJoinPoint().disconnect();
};
this.registerPublisher=function(_2af,_2b0){
dojo.event.connect(_2af,_2b0,this,"sendMessage");
};
this.sendMessage=function(_2b1){
};
};
dojo.provide("dojo.event.browser");
dojo._ie_clobber=new function(){
this.clobberNodes=[];
function nukeProp(node,prop){
try{
node[prop]=null;
}
catch(e){
}
try{
delete node[prop];
}
catch(e){
}
try{
node.removeAttribute(prop);
}
catch(e){
}
}
this.clobber=function(_2b4){
var na;
var tna;
if(_2b4){
tna=_2b4.all||_2b4.getElementsByTagName("*");
na=[_2b4];
for(var x=0;x<tna.length;x++){
if(tna[x]["__doClobber__"]){
na.push(tna[x]);
}
}
}else{
try{
window.onload=null;
}
catch(e){
}
na=(this.clobberNodes.length)?this.clobberNodes:document.all;
}
tna=null;
var _2b8={};
for(var i=na.length-1;i>=0;i=i-1){
var el=na[i];
try{
if(el&&el["__clobberAttrs__"]){
for(var j=0;j<el.__clobberAttrs__.length;j++){
nukeProp(el,el.__clobberAttrs__[j]);
}
nukeProp(el,"__clobberAttrs__");
nukeProp(el,"__doClobber__");
}
}
catch(e){
}
}
na=null;
};
};
if(dojo.render.html.ie){
dojo.addOnUnload(function(){
dojo._ie_clobber.clobber();
try{
if((dojo["widget"])&&(dojo.widget["manager"])){
dojo.widget.manager.destroyAll();
}
}
catch(e){
}
if(dojo.widget){
for(var name in dojo.widget._templateCache){
if(dojo.widget._templateCache[name].node){
dojo.dom.destroyNode(dojo.widget._templateCache[name].node);
dojo.widget._templateCache[name].node=null;
delete dojo.widget._templateCache[name].node;
}
}
}
try{
window.onload=null;
}
catch(e){
}
try{
window.onunload=null;
}
catch(e){
}
dojo._ie_clobber.clobberNodes=[];
});
}
dojo.event.browser=new function(){
var _2bd=0;
this.normalizedEventName=function(_2be){
switch(_2be){
case "CheckboxStateChange":
case "DOMAttrModified":
case "DOMMenuItemActive":
case "DOMMenuItemInactive":
case "DOMMouseScroll":
case "DOMNodeInserted":
case "DOMNodeRemoved":
case "RadioStateChange":
return _2be;
break;
default:
return _2be.toLowerCase();
break;
}
};
this.clean=function(node){
if(dojo.render.html.ie){
dojo._ie_clobber.clobber(node);
}
};
this.addClobberNode=function(node){
if(!dojo.render.html.ie){
return;
}
if(!node["__doClobber__"]){
node.__doClobber__=true;
dojo._ie_clobber.clobberNodes.push(node);
node.__clobberAttrs__=[];
}
};
this.addClobberNodeAttrs=function(node,_2c2){
if(!dojo.render.html.ie){
return;
}
this.addClobberNode(node);
for(var x=0;x<_2c2.length;x++){
node.__clobberAttrs__.push(_2c2[x]);
}
};
this.removeListener=function(node,_2c5,fp,_2c7){
if(!_2c7){
var _2c7=false;
}
_2c5=dojo.event.browser.normalizedEventName(_2c5);
if((_2c5=="onkey")||(_2c5=="key")){
if(dojo.render.html.ie){
this.removeListener(node,"onkeydown",fp,_2c7);
}
_2c5="onkeypress";
}
if(_2c5.substr(0,2)=="on"){
_2c5=_2c5.substr(2);
}
if(node.removeEventListener){
node.removeEventListener(_2c5,fp,_2c7);
}
};
this.addListener=function(node,_2c9,fp,_2cb,_2cc){
if(!node){
return;
}
if(!_2cb){
var _2cb=false;
}
_2c9=dojo.event.browser.normalizedEventName(_2c9);
if((_2c9=="onkey")||(_2c9=="key")){
if(dojo.render.html.ie){
this.addListener(node,"onkeydown",fp,_2cb,_2cc);
}
_2c9="onkeypress";
}
if(_2c9.substr(0,2)!="on"){
_2c9="on"+_2c9;
}
if(!_2cc){
var _2cd=function(evt){
if(!evt){
evt=window.event;
}
var ret=fp(dojo.event.browser.fixEvent(evt,this));
if(_2cb){
dojo.event.browser.stopEvent(evt);
}
return ret;
};
}else{
_2cd=fp;
}
if(node.addEventListener){
node.addEventListener(_2c9.substr(2),_2cd,_2cb);
return _2cd;
}else{
if(typeof node[_2c9]=="function"){
var _2d0=node[_2c9];
node[_2c9]=function(e){
_2d0(e);
return _2cd(e);
};
}else{
node[_2c9]=_2cd;
}
if(dojo.render.html.ie){
this.addClobberNodeAttrs(node,[_2c9]);
}
return _2cd;
}
};
this.isEvent=function(obj){
return (typeof obj!="undefined")&&(obj)&&(typeof Event!="undefined")&&(obj.eventPhase);
};
this.currentEvent=null;
this.callListener=function(_2d3,_2d4){
if(typeof _2d3!="function"){
dojo.raise("listener not a function: "+_2d3);
}
dojo.event.browser.currentEvent.currentTarget=_2d4;
return _2d3.call(_2d4,dojo.event.browser.currentEvent);
};
this._stopPropagation=function(){
dojo.event.browser.currentEvent.cancelBubble=true;
};
this._preventDefault=function(){
dojo.event.browser.currentEvent.returnValue=false;
};
this.keys={KEY_BACKSPACE:8,KEY_TAB:9,KEY_CLEAR:12,KEY_ENTER:13,KEY_SHIFT:16,KEY_CTRL:17,KEY_ALT:18,KEY_PAUSE:19,KEY_CAPS_LOCK:20,KEY_ESCAPE:27,KEY_SPACE:32,KEY_PAGE_UP:33,KEY_PAGE_DOWN:34,KEY_END:35,KEY_HOME:36,KEY_LEFT_ARROW:37,KEY_UP_ARROW:38,KEY_RIGHT_ARROW:39,KEY_DOWN_ARROW:40,KEY_INSERT:45,KEY_DELETE:46,KEY_HELP:47,KEY_LEFT_WINDOW:91,KEY_RIGHT_WINDOW:92,KEY_SELECT:93,KEY_NUMPAD_0:96,KEY_NUMPAD_1:97,KEY_NUMPAD_2:98,KEY_NUMPAD_3:99,KEY_NUMPAD_4:100,KEY_NUMPAD_5:101,KEY_NUMPAD_6:102,KEY_NUMPAD_7:103,KEY_NUMPAD_8:104,KEY_NUMPAD_9:105,KEY_NUMPAD_MULTIPLY:106,KEY_NUMPAD_PLUS:107,KEY_NUMPAD_ENTER:108,KEY_NUMPAD_MINUS:109,KEY_NUMPAD_PERIOD:110,KEY_NUMPAD_DIVIDE:111,KEY_F1:112,KEY_F2:113,KEY_F3:114,KEY_F4:115,KEY_F5:116,KEY_F6:117,KEY_F7:118,KEY_F8:119,KEY_F9:120,KEY_F10:121,KEY_F11:122,KEY_F12:123,KEY_F13:124,KEY_F14:125,KEY_F15:126,KEY_NUM_LOCK:144,KEY_SCROLL_LOCK:145};
this.revKeys=[];
for(var key in this.keys){
this.revKeys[this.keys[key]]=key;
}
this.fixEvent=function(evt,_2d7){
if(!evt){
if(window["event"]){
evt=window.event;
}
}
if((evt["type"])&&(evt["type"].indexOf("key")==0)){
evt.keys=this.revKeys;
for(var key in this.keys){
evt[key]=this.keys[key];
}
if(evt["type"]=="keydown"&&dojo.render.html.ie){
switch(evt.keyCode){
case evt.KEY_SHIFT:
case evt.KEY_CTRL:
case evt.KEY_ALT:
case evt.KEY_CAPS_LOCK:
case evt.KEY_LEFT_WINDOW:
case evt.KEY_RIGHT_WINDOW:
case evt.KEY_SELECT:
case evt.KEY_NUM_LOCK:
case evt.KEY_SCROLL_LOCK:
case evt.KEY_NUMPAD_0:
case evt.KEY_NUMPAD_1:
case evt.KEY_NUMPAD_2:
case evt.KEY_NUMPAD_3:
case evt.KEY_NUMPAD_4:
case evt.KEY_NUMPAD_5:
case evt.KEY_NUMPAD_6:
case evt.KEY_NUMPAD_7:
case evt.KEY_NUMPAD_8:
case evt.KEY_NUMPAD_9:
case evt.KEY_NUMPAD_PERIOD:
break;
case evt.KEY_NUMPAD_MULTIPLY:
case evt.KEY_NUMPAD_PLUS:
case evt.KEY_NUMPAD_ENTER:
case evt.KEY_NUMPAD_MINUS:
case evt.KEY_NUMPAD_DIVIDE:
break;
case evt.KEY_PAUSE:
case evt.KEY_TAB:
case evt.KEY_BACKSPACE:
case evt.KEY_ENTER:
case evt.KEY_ESCAPE:
case evt.KEY_PAGE_UP:
case evt.KEY_PAGE_DOWN:
case evt.KEY_END:
case evt.KEY_HOME:
case evt.KEY_LEFT_ARROW:
case evt.KEY_UP_ARROW:
case evt.KEY_RIGHT_ARROW:
case evt.KEY_DOWN_ARROW:
case evt.KEY_INSERT:
case evt.KEY_DELETE:
case evt.KEY_F1:
case evt.KEY_F2:
case evt.KEY_F3:
case evt.KEY_F4:
case evt.KEY_F5:
case evt.KEY_F6:
case evt.KEY_F7:
case evt.KEY_F8:
case evt.KEY_F9:
case evt.KEY_F10:
case evt.KEY_F11:
case evt.KEY_F12:
case evt.KEY_F12:
case evt.KEY_F13:
case evt.KEY_F14:
case evt.KEY_F15:
case evt.KEY_CLEAR:
case evt.KEY_HELP:
evt.key=evt.keyCode;
break;
default:
if(evt.ctrlKey||evt.altKey){
var _2d9=evt.keyCode;
if(_2d9>=65&&_2d9<=90&&evt.shiftKey==false){
_2d9+=32;
}
if(_2d9>=1&&_2d9<=26&&evt.ctrlKey){
_2d9+=96;
}
evt.key=String.fromCharCode(_2d9);
}
}
}else{
if(evt["type"]=="keypress"){
if(dojo.render.html.opera){
if(evt.which==0){
evt.key=evt.keyCode;
}else{
if(evt.which>0){
switch(evt.which){
case evt.KEY_SHIFT:
case evt.KEY_CTRL:
case evt.KEY_ALT:
case evt.KEY_CAPS_LOCK:
case evt.KEY_NUM_LOCK:
case evt.KEY_SCROLL_LOCK:
break;
case evt.KEY_PAUSE:
case evt.KEY_TAB:
case evt.KEY_BACKSPACE:
case evt.KEY_ENTER:
case evt.KEY_ESCAPE:
evt.key=evt.which;
break;
default:
var _2d9=evt.which;
if((evt.ctrlKey||evt.altKey||evt.metaKey)&&(evt.which>=65&&evt.which<=90&&evt.shiftKey==false)){
_2d9+=32;
}
evt.key=String.fromCharCode(_2d9);
}
}
}
}else{
if(dojo.render.html.ie){
if(!evt.ctrlKey&&!evt.altKey&&evt.keyCode>=evt.KEY_SPACE){
evt.key=String.fromCharCode(evt.keyCode);
}
}else{
if(dojo.render.html.safari){
switch(evt.keyCode){
case 25:
evt.key=evt.KEY_TAB;
evt.shift=true;
break;
case 63232:
evt.key=evt.KEY_UP_ARROW;
break;
case 63233:
evt.key=evt.KEY_DOWN_ARROW;
break;
case 63234:
evt.key=evt.KEY_LEFT_ARROW;
break;
case 63235:
evt.key=evt.KEY_RIGHT_ARROW;
break;
case 63236:
evt.key=evt.KEY_F1;
break;
case 63237:
evt.key=evt.KEY_F2;
break;
case 63238:
evt.key=evt.KEY_F3;
break;
case 63239:
evt.key=evt.KEY_F4;
break;
case 63240:
evt.key=evt.KEY_F5;
break;
case 63241:
evt.key=evt.KEY_F6;
break;
case 63242:
evt.key=evt.KEY_F7;
break;
case 63243:
evt.key=evt.KEY_F8;
break;
case 63244:
evt.key=evt.KEY_F9;
break;
case 63245:
evt.key=evt.KEY_F10;
break;
case 63246:
evt.key=evt.KEY_F11;
break;
case 63247:
evt.key=evt.KEY_F12;
break;
case 63250:
evt.key=evt.KEY_PAUSE;
break;
case 63272:
evt.key=evt.KEY_DELETE;
break;
case 63273:
evt.key=evt.KEY_HOME;
break;
case 63275:
evt.key=evt.KEY_END;
break;
case 63276:
evt.key=evt.KEY_PAGE_UP;
break;
case 63277:
evt.key=evt.KEY_PAGE_DOWN;
break;
case 63302:
evt.key=evt.KEY_INSERT;
break;
case 63248:
case 63249:
case 63289:
break;
default:
evt.key=evt.charCode>=evt.KEY_SPACE?String.fromCharCode(evt.charCode):evt.keyCode;
}
}else{
evt.key=evt.charCode>0?String.fromCharCode(evt.charCode):evt.keyCode;
}
}
}
}
}
}
if(dojo.render.html.ie){
if(!evt.target){
evt.target=evt.srcElement;
}
if(!evt.currentTarget){
evt.currentTarget=(_2d7?_2d7:evt.srcElement);
}
if(!evt.layerX){
evt.layerX=evt.offsetX;
}
if(!evt.layerY){
evt.layerY=evt.offsetY;
}
var doc=(evt.srcElement&&evt.srcElement.ownerDocument)?evt.srcElement.ownerDocument:document;
var _2db=((dojo.render.html.ie55)||(doc["compatMode"]=="BackCompat"))?doc.body:doc.documentElement;
if(!evt.pageX){
evt.pageX=evt.clientX+(_2db.scrollLeft||0);
}
if(!evt.pageY){
evt.pageY=evt.clientY+(_2db.scrollTop||0);
}
if(evt.type=="mouseover"){
evt.relatedTarget=evt.fromElement;
}
if(evt.type=="mouseout"){
evt.relatedTarget=evt.toElement;
}
this.currentEvent=evt;
evt.callListener=this.callListener;
evt.stopPropagation=this._stopPropagation;
evt.preventDefault=this._preventDefault;
}
return evt;
};
this.stopEvent=function(evt){
if(window.event){
evt.cancelBubble=true;
evt.returnValue=false;
}else{
evt.preventDefault();
evt.stopPropagation();
}
};
};
dojo.kwCompoundRequire({common:["dojo.event.common","dojo.event.topic"],browser:["dojo.event.browser"],dashboard:["dojo.event.browser"]});
dojo.provide("dojo.event.*");
dojo.provide("dojo.widget.Manager");
dojo.widget.manager=new function(){
this.widgets=[];
this.widgetIds=[];
this.topWidgets={};
var _2dd={};
var _2de=[];
this.getUniqueId=function(_2df){
var _2e0;
do{
_2e0=_2df+"_"+(_2dd[_2df]!=undefined?++_2dd[_2df]:_2dd[_2df]=0);
}while(this.getWidgetById(_2e0));
return _2e0;
};
this.add=function(_2e1){
this.widgets.push(_2e1);
if(!_2e1.extraArgs["id"]){
_2e1.extraArgs["id"]=_2e1.extraArgs["ID"];
}
if(_2e1.widgetId==""){
if(_2e1["id"]){
_2e1.widgetId=_2e1["id"];
}else{
if(_2e1.extraArgs["id"]){
_2e1.widgetId=_2e1.extraArgs["id"];
}else{
_2e1.widgetId=this.getUniqueId(_2e1.ns+"_"+_2e1.widgetType);
}
}
}
if(this.widgetIds[_2e1.widgetId]){
dojo.debug("widget ID collision on ID: "+_2e1.widgetId);
}
this.widgetIds[_2e1.widgetId]=_2e1;
};
this.destroyAll=function(){
for(var x=this.widgets.length-1;x>=0;x--){
try{
this.widgets[x].destroy(true);
delete this.widgets[x];
}
catch(e){
}
}
};
this.remove=function(_2e3){
if(dojo.lang.isNumber(_2e3)){
var tw=this.widgets[_2e3].widgetId;
delete this.topWidgets[tw];
delete this.widgetIds[tw];
this.widgets.splice(_2e3,1);
}else{
this.removeById(_2e3);
}
};
this.removeById=function(id){
if(!dojo.lang.isString(id)){
id=id["widgetId"];
if(!id){
dojo.debug("invalid widget or id passed to removeById");
return;
}
}
for(var i=0;i<this.widgets.length;i++){
if(this.widgets[i].widgetId==id){
this.remove(i);
break;
}
}
};
this.getWidgetById=function(id){
if(dojo.lang.isString(id)){
return this.widgetIds[id];
}
return id;
};
this.getWidgetsByType=function(type){
var lt=type.toLowerCase();
var _2ea=(type.indexOf(":")<0?function(x){
return x.widgetType.toLowerCase();
}:function(x){
return x.getNamespacedType();
});
var ret=[];
dojo.lang.forEach(this.widgets,function(x){
if(_2ea(x)==lt){
ret.push(x);
}
});
return ret;
};
this.getWidgetsByFilter=function(_2ef,_2f0){
var ret=[];
dojo.lang.every(this.widgets,function(x){
if(_2ef(x)){
ret.push(x);
if(_2f0){
return false;
}
}
return true;
});
return (_2f0?ret[0]:ret);
};
this.getAllWidgets=function(){
return this.widgets.concat();
};
this.getWidgetByNode=function(node){
var w=this.getAllWidgets();
node=dojo.byId(node);
for(var i=0;i<w.length;i++){
if(w[i].domNode==node){
return w[i];
}
}
return null;
};
this.byId=this.getWidgetById;
this.byType=this.getWidgetsByType;
this.byFilter=this.getWidgetsByFilter;
this.byNode=this.getWidgetByNode;
var _2f6={};
var _2f7=["dojo.widget"];
for(var i=0;i<_2f7.length;i++){
_2f7[_2f7[i]]=true;
}
this.registerWidgetPackage=function(_2f9){
if(!_2f7[_2f9]){
_2f7[_2f9]=true;
_2f7.push(_2f9);
}
};
this.getWidgetPackageList=function(){
return dojo.lang.map(_2f7,function(elt){
return (elt!==true?elt:undefined);
});
};
this.getImplementation=function(_2fb,_2fc,_2fd,ns){
var impl=this.getImplementationName(_2fb,ns);
if(impl){
var ret=_2fc?new impl(_2fc):new impl();
return ret;
}
};
function buildPrefixCache(){
for(var _301 in dojo.render){
if(dojo.render[_301]["capable"]===true){
var _302=dojo.render[_301].prefixes;
for(var i=0;i<_302.length;i++){
_2de.push(_302[i].toLowerCase());
}
}
}
}
var _304=function(_305,_306){
if(!_306){
return null;
}
for(var i=0,l=_2de.length,_309;i<=l;i++){
_309=(i<l?_306[_2de[i]]:_306);
if(!_309){
continue;
}
for(var name in _309){
if(name.toLowerCase()==_305){
return _309[name];
}
}
}
return null;
};
var _30b=function(_30c,_30d){
var _30e=dojo.evalObjPath(_30d,false);
return (_30e?_304(_30c,_30e):null);
};
this.getImplementationName=function(_30f,ns){
var _311=_30f.toLowerCase();
ns=ns||"dojo";
var imps=_2f6[ns]||(_2f6[ns]={});
var impl=imps[_311];
if(impl){
return impl;
}
if(!_2de.length){
buildPrefixCache();
}
var _314=dojo.ns.get(ns);
if(!_314){
dojo.ns.register(ns,ns+".widget");
_314=dojo.ns.get(ns);
}
if(_314){
_314.resolve(_30f);
}
impl=_30b(_311,_314.module);
if(impl){
return (imps[_311]=impl);
}
_314=dojo.ns.require(ns);
if((_314)&&(_314.resolver)){
_314.resolve(_30f);
impl=_30b(_311,_314.module);
if(impl){
return (imps[_311]=impl);
}
}
dojo.deprecated("dojo.widget.Manager.getImplementationName","Could not locate widget implementation for \""+_30f+"\" in \""+_314.module+"\" registered to namespace \""+_314.name+"\". "+"Developers must specify correct namespaces for all non-Dojo widgets","0.5");
for(var i=0;i<_2f7.length;i++){
impl=_30b(_311,_2f7[i]);
if(impl){
return (imps[_311]=impl);
}
}
throw new Error("Could not locate widget implementation for \""+_30f+"\" in \""+_314.module+"\" registered to namespace \""+_314.name+"\"");
};
this.resizing=false;
this.onWindowResized=function(){
if(this.resizing){
return;
}
try{
this.resizing=true;
for(var id in this.topWidgets){
var _317=this.topWidgets[id];
if(_317.checkSize){
_317.checkSize();
}
}
}
catch(e){
}
finally{
this.resizing=false;
}
};
if(typeof window!="undefined"){
dojo.addOnLoad(this,"onWindowResized");
dojo.event.connect(window,"onresize",this,"onWindowResized");
}
};
(function(){
var dw=dojo.widget;
var dwm=dw.manager;
var h=dojo.lang.curry(dojo.lang,"hitch",dwm);
var g=function(_31c,_31d){
dw[(_31d||_31c)]=h(_31c);
};
g("add","addWidget");
g("destroyAll","destroyAllWidgets");
g("remove","removeWidget");
g("removeById","removeWidgetById");
g("getWidgetById");
g("getWidgetById","byId");
g("getWidgetsByType");
g("getWidgetsByFilter");
g("getWidgetsByType","byType");
g("getWidgetsByFilter","byFilter");
g("getWidgetByNode","byNode");
dw.all=function(n){
var _31f=dwm.getAllWidgets.apply(dwm,arguments);
if(arguments.length>0){
return _31f[n];
}
return _31f;
};
g("registerWidgetPackage");
g("getImplementation","getWidgetImplementation");
g("getImplementationName","getWidgetImplementationName");
dw.widgets=dwm.widgets;
dw.widgetIds=dwm.widgetIds;
dw.root=dwm.root;
})();
dojo.provide("dojo.uri.Uri");
dojo.uri=new function(){
this.dojoUri=function(uri){
return new dojo.uri.Uri(dojo.hostenv.getBaseScriptUri(),uri);
};
this.moduleUri=function(_321,uri){
var loc=dojo.hostenv.getModuleSymbols(_321).join("/");
if(!loc){
return null;
}
if(loc.lastIndexOf("/")!=loc.length-1){
loc+="/";
}
var _324=loc.indexOf(":");
var _325=loc.indexOf("/");
if(loc.charAt(0)!="/"&&(_324==-1||_324>_325)){
loc=dojo.hostenv.getBaseScriptUri()+loc;
}
return new dojo.uri.Uri(loc,uri);
};
this.Uri=function(){
var uri=arguments[0];
for(var i=1;i<arguments.length;i++){
if(!arguments[i]){
continue;
}
var _328=new dojo.uri.Uri(arguments[i].toString());
var _329=new dojo.uri.Uri(uri.toString());
if((_328.path=="")&&(_328.scheme==null)&&(_328.authority==null)&&(_328.query==null)){
if(_328.fragment!=null){
_329.fragment=_328.fragment;
}
_328=_329;
}else{
if(_328.scheme==null){
_328.scheme=_329.scheme;
if(_328.authority==null){
_328.authority=_329.authority;
if(_328.path.charAt(0)!="/"){
var path=_329.path.substring(0,_329.path.lastIndexOf("/")+1)+_328.path;
var segs=path.split("/");
for(var j=0;j<segs.length;j++){
if(segs[j]=="."){
if(j==segs.length-1){
segs[j]="";
}else{
segs.splice(j,1);
j--;
}
}else{
if(j>0&&!(j==1&&segs[0]=="")&&segs[j]==".."&&segs[j-1]!=".."){
if(j==segs.length-1){
segs.splice(j,1);
segs[j-1]="";
}else{
segs.splice(j-1,2);
j-=2;
}
}
}
}
_328.path=segs.join("/");
}
}
}
}
uri="";
if(_328.scheme!=null){
uri+=_328.scheme+":";
}
if(_328.authority!=null){
uri+="//"+_328.authority;
}
uri+=_328.path;
if(_328.query!=null){
uri+="?"+_328.query;
}
if(_328.fragment!=null){
uri+="#"+_328.fragment;
}
}
this.uri=uri.toString();
var _32d="^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?$";
var r=this.uri.match(new RegExp(_32d));
this.scheme=r[2]||(r[1]?"":null);
this.authority=r[4]||(r[3]?"":null);
this.path=r[5];
this.query=r[7]||(r[6]?"":null);
this.fragment=r[9]||(r[8]?"":null);
if(this.authority!=null){
_32d="^((([^:]+:)?([^@]+))@)?([^:]*)(:([0-9]+))?$";
r=this.authority.match(new RegExp(_32d));
this.user=r[3]||null;
this.password=r[4]||null;
this.host=r[5];
this.port=r[7]||null;
}
this.toString=function(){
return this.uri;
};
};
};
dojo.kwCompoundRequire({common:[["dojo.uri.Uri",false,false]]});
dojo.provide("dojo.uri.*");
dojo.provide("dojo.html.common");
dojo.lang.mixin(dojo.html,dojo.dom);
dojo.html.body=function(){
dojo.deprecated("dojo.html.body() moved to dojo.body()","0.5");
return dojo.body();
};
dojo.html.getEventTarget=function(evt){
if(!evt){
evt=dojo.global().event||{};
}
var t=(evt.srcElement?evt.srcElement:(evt.target?evt.target:null));
while((t)&&(t.nodeType!=1)){
t=t.parentNode;
}
return t;
};
dojo.html.getViewport=function(){
var _331=dojo.global();
var _332=dojo.doc();
var w=0;
var h=0;
if(dojo.render.html.mozilla){
w=_332.documentElement.clientWidth;
h=_331.innerHeight;
}else{
if(!dojo.render.html.opera&&_331.innerWidth){
w=_331.innerWidth;
h=_331.innerHeight;
}else{
if(!dojo.render.html.opera&&dojo.exists(_332,"documentElement.clientWidth")){
var w2=_332.documentElement.clientWidth;
if(!w||w2&&w2<w){
w=w2;
}
h=_332.documentElement.clientHeight;
}else{
if(dojo.body().clientWidth){
w=dojo.body().clientWidth;
h=dojo.body().clientHeight;
}
}
}
}
return {width:w,height:h};
};
dojo.html.getScroll=function(){
var _336=dojo.global();
var _337=dojo.doc();
var top=_336.pageYOffset||_337.documentElement.scrollTop||dojo.body().scrollTop||0;
var left=_336.pageXOffset||_337.documentElement.scrollLeft||dojo.body().scrollLeft||0;
return {top:top,left:left,offset:{x:left,y:top}};
};
dojo.html.getParentByType=function(node,type){
var _33c=dojo.doc();
var _33d=dojo.byId(node);
type=type.toLowerCase();
while((_33d)&&(_33d.nodeName.toLowerCase()!=type)){
if(_33d==(_33c["body"]||_33c["documentElement"])){
return null;
}
_33d=_33d.parentNode;
}
return _33d;
};
dojo.html.getAttribute=function(node,attr){
node=dojo.byId(node);
if((!node)||(!node.getAttribute)){
return null;
}
var ta=typeof attr=="string"?attr:new String(attr);
var v=node.getAttribute(ta.toUpperCase());
if((v)&&(typeof v=="string")&&(v!="")){
return v;
}
if(v&&v.value){
return v.value;
}
if((node.getAttributeNode)&&(node.getAttributeNode(ta))){
return (node.getAttributeNode(ta)).value;
}else{
if(node.getAttribute(ta)){
return node.getAttribute(ta);
}else{
if(node.getAttribute(ta.toLowerCase())){
return node.getAttribute(ta.toLowerCase());
}
}
}
return null;
};
dojo.html.hasAttribute=function(node,attr){
return dojo.html.getAttribute(dojo.byId(node),attr)?true:false;
};
dojo.html.getCursorPosition=function(e){
e=e||dojo.global().event;
var _345={x:0,y:0};
if(e.pageX||e.pageY){
_345.x=e.pageX;
_345.y=e.pageY;
}else{
var de=dojo.doc().documentElement;
var db=dojo.body();
_345.x=e.clientX+((de||db)["scrollLeft"])-((de||db)["clientLeft"]);
_345.y=e.clientY+((de||db)["scrollTop"])-((de||db)["clientTop"]);
}
return _345;
};
dojo.html.isTag=function(node){
node=dojo.byId(node);
if(node&&node.tagName){
for(var i=1;i<arguments.length;i++){
if(node.tagName.toLowerCase()==String(arguments[i]).toLowerCase()){
return String(arguments[i]).toLowerCase();
}
}
}
return "";
};
if(dojo.render.html.ie&&!dojo.render.html.ie70){
if(window.location.href.substr(0,6).toLowerCase()!="https:"){
(function(){
var _34a=dojo.doc().createElement("script");
_34a.src="javascript:'dojo.html.createExternalElement=function(doc, tag){ return doc.createElement(tag); }'";
dojo.doc().getElementsByTagName("head")[0].appendChild(_34a);
})();
}
}else{
dojo.html.createExternalElement=function(doc,tag){
return doc.createElement(tag);
};
}
dojo.html._callDeprecated=function(_34d,_34e,args,_350,_351){
dojo.deprecated("dojo.html."+_34d,"replaced by dojo.html."+_34e+"("+(_350?"node, {"+_350+": "+_350+"}":"")+")"+(_351?"."+_351:""),"0.5");
var _352=[];
if(_350){
var _353={};
_353[_350]=args[1];
_352.push(args[0]);
_352.push(_353);
}else{
_352=args;
}
var ret=dojo.html[_34e].apply(dojo.html,args);
if(_351){
return ret[_351];
}else{
return ret;
}
};
dojo.html.getViewportWidth=function(){
return dojo.html._callDeprecated("getViewportWidth","getViewport",arguments,null,"width");
};
dojo.html.getViewportHeight=function(){
return dojo.html._callDeprecated("getViewportHeight","getViewport",arguments,null,"height");
};
dojo.html.getViewportSize=function(){
return dojo.html._callDeprecated("getViewportSize","getViewport",arguments);
};
dojo.html.getScrollTop=function(){
return dojo.html._callDeprecated("getScrollTop","getScroll",arguments,null,"top");
};
dojo.html.getScrollLeft=function(){
return dojo.html._callDeprecated("getScrollLeft","getScroll",arguments,null,"left");
};
dojo.html.getScrollOffset=function(){
return dojo.html._callDeprecated("getScrollOffset","getScroll",arguments,null,"offset");
};
dojo.provide("dojo.a11y");
dojo.a11y={imgPath:dojo.uri.moduleUri("dojo.widget","templates/images"),doAccessibleCheck:true,accessible:null,checkAccessible:function(){
if(this.accessible===null){
this.accessible=false;
if(this.doAccessibleCheck==true){
this.accessible=this.testAccessible();
}
}
return this.accessible;
},testAccessible:function(){
this.accessible=false;
if(dojo.render.html.ie||dojo.render.html.mozilla){
var div=document.createElement("div");
div.style.backgroundImage="url(\""+this.imgPath+"/tab_close.gif\")";
dojo.body().appendChild(div);
var _356=null;
if(window.getComputedStyle){
var _357=getComputedStyle(div,"");
_356=_357.getPropertyValue("background-image");
}else{
_356=div.currentStyle.backgroundImage;
}
var _358=false;
if(_356!=null&&(_356=="none"||_356=="url(invalid-url:)")){
this.accessible=true;
}
dojo.body().removeChild(div);
}
return this.accessible;
},setCheckAccessible:function(_359){
this.doAccessibleCheck=_359;
},setAccessibleMode:function(){
if(this.accessible===null){
if(this.checkAccessible()){
dojo.render.html.prefixes.unshift("a11y");
}
}
return this.accessible;
}};
dojo.provide("dojo.widget.Widget");
dojo.declare("dojo.widget.Widget",null,function(){
this.children=[];
this.extraArgs={};
},{parent:null,isTopLevel:false,disabled:false,isContainer:false,widgetId:"",widgetType:"Widget",ns:"dojo",getNamespacedType:function(){
return (this.ns?this.ns+":"+this.widgetType:this.widgetType).toLowerCase();
},toString:function(){
return "[Widget "+this.getNamespacedType()+", "+(this.widgetId||"NO ID")+"]";
},repr:function(){
return this.toString();
},enable:function(){
this.disabled=false;
},disable:function(){
this.disabled=true;
},onResized:function(){
this.notifyChildrenOfResize();
},notifyChildrenOfResize:function(){
for(var i=0;i<this.children.length;i++){
var _35b=this.children[i];
if(_35b.onResized){
_35b.onResized();
}
}
},create:function(args,_35d,_35e,ns){
if(ns){
this.ns=ns;
}
this.satisfyPropertySets(args,_35d,_35e);
this.mixInProperties(args,_35d,_35e);
this.postMixInProperties(args,_35d,_35e);
dojo.widget.manager.add(this);
this.buildRendering(args,_35d,_35e);
this.initialize(args,_35d,_35e);
this.postInitialize(args,_35d,_35e);
this.postCreate(args,_35d,_35e);
return this;
},destroy:function(_360){
if(this.parent){
this.parent.removeChild(this);
}
this.destroyChildren();
this.uninitialize();
this.destroyRendering(_360);
dojo.widget.manager.removeById(this.widgetId);
},destroyChildren:function(){
var _361;
var i=0;
while(this.children.length>i){
_361=this.children[i];
if(_361 instanceof dojo.widget.Widget){
this.removeChild(_361);
_361.destroy();
continue;
}
i++;
}
},getChildrenOfType:function(type,_364){
var ret=[];
var _366=dojo.lang.isFunction(type);
if(!_366){
type=type.toLowerCase();
}
for(var x=0;x<this.children.length;x++){
if(_366){
if(this.children[x] instanceof type){
ret.push(this.children[x]);
}
}else{
if(this.children[x].widgetType.toLowerCase()==type){
ret.push(this.children[x]);
}
}
if(_364){
ret=ret.concat(this.children[x].getChildrenOfType(type,_364));
}
}
return ret;
},getDescendants:function(){
var _368=[];
var _369=[this];
var elem;
while((elem=_369.pop())){
_368.push(elem);
if(elem.children){
dojo.lang.forEach(elem.children,function(elem){
_369.push(elem);
});
}
}
return _368;
},isFirstChild:function(){
return this===this.parent.children[0];
},isLastChild:function(){
return this===this.parent.children[this.parent.children.length-1];
},satisfyPropertySets:function(args){
return args;
},mixInProperties:function(args,frag){
if((args["fastMixIn"])||(frag["fastMixIn"])){
for(var x in args){
this[x]=args[x];
}
return;
}
var _370;
var _371=dojo.widget.lcArgsCache[this.widgetType];
if(_371==null){
_371={};
for(var y in this){
_371[((new String(y)).toLowerCase())]=y;
}
dojo.widget.lcArgsCache[this.widgetType]=_371;
}
var _373={};
for(var x in args){
if(!this[x]){
var y=_371[(new String(x)).toLowerCase()];
if(y){
args[y]=args[x];
x=y;
}
}
if(_373[x]){
continue;
}
_373[x]=true;
if((typeof this[x])!=(typeof _370)){
if(typeof args[x]!="string"){
this[x]=args[x];
}else{
if(dojo.lang.isString(this[x])){
this[x]=args[x];
}else{
if(dojo.lang.isNumber(this[x])){
this[x]=new Number(args[x]);
}else{
if(dojo.lang.isBoolean(this[x])){
this[x]=(args[x].toLowerCase()=="false")?false:true;
}else{
if(dojo.lang.isFunction(this[x])){
if(args[x].search(/[^\w\.]+/i)==-1){
this[x]=dojo.evalObjPath(args[x],false);
}else{
var tn=dojo.lang.nameAnonFunc(new Function(args[x]),this);
dojo.event.kwConnect({srcObj:this,srcFunc:x,adviceObj:this,adviceFunc:tn});
}
}else{
if(dojo.lang.isArray(this[x])){
this[x]=args[x].split(";");
}else{
if(this[x] instanceof Date){
this[x]=new Date(Number(args[x]));
}else{
if(typeof this[x]=="object"){
if(this[x] instanceof dojo.uri.Uri){
this[x]=dojo.uri.dojoUri(args[x]);
}else{
var _375=args[x].split(";");
for(var y=0;y<_375.length;y++){
var si=_375[y].indexOf(":");
if((si!=-1)&&(_375[y].length>si)){
this[x][_375[y].substr(0,si).replace(/^\s+|\s+$/g,"")]=_375[y].substr(si+1);
}
}
}
}else{
this[x]=args[x];
}
}
}
}
}
}
}
}
}else{
this.extraArgs[x.toLowerCase()]=args[x];
}
}
},postMixInProperties:function(args,frag,_379){
},initialize:function(args,frag,_37c){
return false;
},postInitialize:function(args,frag,_37f){
return false;
},postCreate:function(args,frag,_382){
return false;
},uninitialize:function(){
return false;
},buildRendering:function(args,frag,_385){
dojo.unimplemented("dojo.widget.Widget.buildRendering, on "+this.toString()+", ");
return false;
},destroyRendering:function(){
dojo.unimplemented("dojo.widget.Widget.destroyRendering");
return false;
},addedTo:function(_386){
},addChild:function(_387){
dojo.unimplemented("dojo.widget.Widget.addChild");
return false;
},removeChild:function(_388){
for(var x=0;x<this.children.length;x++){
if(this.children[x]===_388){
this.children.splice(x,1);
_388.parent=null;
break;
}
}
return _388;
},getPreviousSibling:function(){
var idx=this.getParentIndex();
if(idx<=0){
return null;
}
return this.parent.children[idx-1];
},getSiblings:function(){
return this.parent.children;
},getParentIndex:function(){
return dojo.lang.indexOf(this.parent.children,this,true);
},getNextSibling:function(){
var idx=this.getParentIndex();
if(idx==this.parent.children.length-1){
return null;
}
if(idx<0){
return null;
}
return this.parent.children[idx+1];
}});
dojo.widget.lcArgsCache={};
dojo.widget.tags={};
dojo.widget.tags.addParseTreeHandler=function(type){
dojo.deprecated("addParseTreeHandler",". ParseTreeHandlers are now reserved for components. Any unfiltered DojoML tag without a ParseTreeHandler is assumed to be a widget","0.5");
};
dojo.widget.tags["dojo:propertyset"]=function(_38d,_38e,_38f){
var _390=_38e.parseProperties(_38d["dojo:propertyset"]);
};
dojo.widget.tags["dojo:connect"]=function(_391,_392,_393){
var _394=_392.parseProperties(_391["dojo:connect"]);
};
dojo.widget.buildWidgetFromParseTree=function(type,frag,_397,_398,_399,_39a){
dojo.a11y.setAccessibleMode();
var _39b=type.split(":");
_39b=(_39b.length==2)?_39b[1]:type;
var _39c=_39a||_397.parseProperties(frag[frag["ns"]+":"+_39b]);
var _39d=dojo.widget.manager.getImplementation(_39b,null,null,frag["ns"]);
if(!_39d){
throw new Error("cannot find \""+type+"\" widget");
}else{
if(!_39d.create){
throw new Error("\""+type+"\" widget object has no \"create\" method and does not appear to implement *Widget");
}
}
_39c["dojoinsertionindex"]=_399;
var ret=_39d.create(_39c,frag,_398,frag["ns"]);
return ret;
};
dojo.widget.defineWidget=function(_39f,_3a0,_3a1,init,_3a3){
if(dojo.lang.isString(arguments[3])){
dojo.widget._defineWidget(arguments[0],arguments[3],arguments[1],arguments[4],arguments[2]);
}else{
var args=[arguments[0]],p=3;
if(dojo.lang.isString(arguments[1])){
args.push(arguments[1],arguments[2]);
}else{
args.push("",arguments[1]);
p=2;
}
if(dojo.lang.isFunction(arguments[p])){
args.push(arguments[p],arguments[p+1]);
}else{
args.push(null,arguments[p]);
}
dojo.widget._defineWidget.apply(this,args);
}
};
dojo.widget.defineWidget.renderers="html|svg|vml";
dojo.widget._defineWidget=function(_3a6,_3a7,_3a8,init,_3aa){
var _3ab=_3a6.split(".");
var type=_3ab.pop();
var regx="\\.("+(_3a7?_3a7+"|":"")+dojo.widget.defineWidget.renderers+")\\.";
var r=_3a6.search(new RegExp(regx));
_3ab=(r<0?_3ab.join("."):_3a6.substr(0,r));
dojo.widget.manager.registerWidgetPackage(_3ab);
var pos=_3ab.indexOf(".");
var _3b0=(pos>-1)?_3ab.substring(0,pos):_3ab;
_3aa=(_3aa)||{};
_3aa.widgetType=type;
if((!init)&&(_3aa["classConstructor"])){
init=_3aa.classConstructor;
delete _3aa.classConstructor;
}
dojo.declare(_3a6,_3a8,init,_3aa);
};
dojo.provide("dojo.widget.Parse");
dojo.widget.Parse=function(_3b1){
this.propertySetsList=[];
this.fragment=_3b1;
this.createComponents=function(frag,_3b3){
var _3b4=[];
var _3b5=false;
try{
if(frag&&frag.tagName&&(frag!=frag.nodeRef)){
var _3b6=dojo.widget.tags;
var tna=String(frag.tagName).split(";");
for(var x=0;x<tna.length;x++){
var ltn=tna[x].replace(/^\s+|\s+$/g,"").toLowerCase();
frag.tagName=ltn;
var ret;
if(_3b6[ltn]){
_3b5=true;
ret=_3b6[ltn](frag,this,_3b3,frag.index);
_3b4.push(ret);
}else{
if(ltn.indexOf(":")==-1){
ltn="dojo:"+ltn;
}
ret=dojo.widget.buildWidgetFromParseTree(ltn,frag,this,_3b3,frag.index);
if(ret){
_3b5=true;
_3b4.push(ret);
}
}
}
}
}
catch(e){
dojo.debug("dojo.widget.Parse: error:",e);
}
if(!_3b5){
_3b4=_3b4.concat(this.createSubComponents(frag,_3b3));
}
return _3b4;
};
this.createSubComponents=function(_3bb,_3bc){
var frag,_3be=[];
for(var item in _3bb){
frag=_3bb[item];
if(frag&&typeof frag=="object"&&(frag!=_3bb.nodeRef)&&(frag!=_3bb.tagName)&&(!dojo.dom.isNode(frag))){
_3be=_3be.concat(this.createComponents(frag,_3bc));
}
}
return _3be;
};
this.parsePropertySets=function(_3c0){
return [];
};
this.parseProperties=function(_3c1){
var _3c2={};
for(var item in _3c1){
if((_3c1[item]==_3c1.tagName)||(_3c1[item]==_3c1.nodeRef)){
}else{
var frag=_3c1[item];
if(frag.tagName&&dojo.widget.tags[frag.tagName.toLowerCase()]){
}else{
if(frag[0]&&frag[0].value!=""&&frag[0].value!=null){
try{
if(item.toLowerCase()=="dataprovider"){
var _3c5=this;
this.getDataProvider(_3c5,frag[0].value);
_3c2.dataProvider=this.dataProvider;
}
_3c2[item]=frag[0].value;
var _3c6=this.parseProperties(frag);
for(var _3c7 in _3c6){
_3c2[_3c7]=_3c6[_3c7];
}
}
catch(e){
dojo.debug(e);
}
}
}
switch(item.toLowerCase()){
case "checked":
case "disabled":
if(typeof _3c2[item]!="boolean"){
_3c2[item]=true;
}
break;
}
}
}
return _3c2;
};
this.getDataProvider=function(_3c8,_3c9){
dojo.io.bind({url:_3c9,load:function(type,_3cb){
if(type=="load"){
_3c8.dataProvider=_3cb;
}
},mimetype:"text/javascript",sync:true});
};
this.getPropertySetById=function(_3cc){
for(var x=0;x<this.propertySetsList.length;x++){
if(_3cc==this.propertySetsList[x]["id"][0].value){
return this.propertySetsList[x];
}
}
return "";
};
this.getPropertySetsByType=function(_3ce){
var _3cf=[];
for(var x=0;x<this.propertySetsList.length;x++){
var cpl=this.propertySetsList[x];
var cpcc=cpl.componentClass||cpl.componentType||null;
var _3d3=this.propertySetsList[x]["id"][0].value;
if(cpcc&&(_3d3==cpcc[0].value)){
_3cf.push(cpl);
}
}
return _3cf;
};
this.getPropertySets=function(_3d4){
var ppl="dojo:propertyproviderlist";
var _3d6=[];
var _3d7=_3d4.tagName;
if(_3d4[ppl]){
var _3d8=_3d4[ppl].value.split(" ");
for(var _3d9 in _3d8){
if((_3d9.indexOf("..")==-1)&&(_3d9.indexOf("://")==-1)){
var _3da=this.getPropertySetById(_3d9);
if(_3da!=""){
_3d6.push(_3da);
}
}else{
}
}
}
return this.getPropertySetsByType(_3d7).concat(_3d6);
};
this.createComponentFromScript=function(_3db,_3dc,_3dd,ns){
_3dd.fastMixIn=true;
var ltn=(ns||"dojo")+":"+_3dc.toLowerCase();
if(dojo.widget.tags[ltn]){
return [dojo.widget.tags[ltn](_3dd,this,null,null,_3dd)];
}
return [dojo.widget.buildWidgetFromParseTree(ltn,_3dd,this,null,null,_3dd)];
};
};
dojo.widget._parser_collection={"dojo":new dojo.widget.Parse()};
dojo.widget.getParser=function(name){
if(!name){
name="dojo";
}
if(!this._parser_collection[name]){
this._parser_collection[name]=new dojo.widget.Parse();
}
return this._parser_collection[name];
};
dojo.widget.createWidget=function(name,_3e2,_3e3,_3e4){
var _3e5=false;
var _3e6=(typeof name=="string");
if(_3e6){
var pos=name.indexOf(":");
var ns=(pos>-1)?name.substring(0,pos):"dojo";
if(pos>-1){
name=name.substring(pos+1);
}
var _3e9=name.toLowerCase();
var _3ea=ns+":"+_3e9;
_3e5=(dojo.byId(name)&&!dojo.widget.tags[_3ea]);
}
if((arguments.length==1)&&(_3e5||!_3e6)){
var xp=new dojo.xml.Parse();
var tn=_3e5?dojo.byId(name):name;
return dojo.widget.getParser().createComponents(xp.parseElement(tn,null,true))[0];
}
function fromScript(_3ed,name,_3ef,ns){
_3ef[_3ea]={dojotype:[{value:_3e9}],nodeRef:_3ed,fastMixIn:true};
_3ef.ns=ns;
return dojo.widget.getParser().createComponentFromScript(_3ed,name,_3ef,ns);
}
_3e2=_3e2||{};
var _3f1=false;
var tn=null;
var h=dojo.render.html.capable;
if(h){
tn=document.createElement("span");
}
if(!_3e3){
_3f1=true;
_3e3=tn;
if(h){
dojo.body().appendChild(_3e3);
}
}else{
if(_3e4){
dojo.dom.insertAtPosition(tn,_3e3,_3e4);
}else{
tn=_3e3;
}
}
var _3f3=fromScript(tn,name.toLowerCase(),_3e2,ns);
if((!_3f3)||(!_3f3[0])||(typeof _3f3[0].widgetType=="undefined")){
throw new Error("createWidget: Creation of \""+name+"\" widget failed.");
}
try{
if(_3f1&&_3f3[0].domNode.parentNode){
_3f3[0].domNode.parentNode.removeChild(_3f3[0].domNode);
}
}
catch(e){
dojo.debug(e);
}
return _3f3[0];
};
dojo.provide("dojo.html.style");
dojo.html.getClass=function(node){
node=dojo.byId(node);
if(!node){
return "";
}
var cs="";
if(node.className){
cs=node.className;
}else{
if(dojo.html.hasAttribute(node,"class")){
cs=dojo.html.getAttribute(node,"class");
}
}
return cs.replace(/^\s+|\s+$/g,"");
};
dojo.html.getClasses=function(node){
var c=dojo.html.getClass(node);
return (c=="")?[]:c.split(/\s+/g);
};
dojo.html.hasClass=function(node,_3f9){
return (new RegExp("(^|\\s+)"+_3f9+"(\\s+|$)")).test(dojo.html.getClass(node));
};
dojo.html.prependClass=function(node,_3fb){
_3fb+=" "+dojo.html.getClass(node);
return dojo.html.setClass(node,_3fb);
};
dojo.html.addClass=function(node,_3fd){
if(dojo.html.hasClass(node,_3fd)){
return false;
}
_3fd=(dojo.html.getClass(node)+" "+_3fd).replace(/^\s+|\s+$/g,"");
return dojo.html.setClass(node,_3fd);
};
dojo.html.setClass=function(node,_3ff){
node=dojo.byId(node);
var cs=new String(_3ff);
try{
if(typeof node.className=="string"){
node.className=cs;
}else{
if(node.setAttribute){
node.setAttribute("class",_3ff);
node.className=cs;
}else{
return false;
}
}
}
catch(e){
dojo.debug("dojo.html.setClass() failed",e);
}
return true;
};
dojo.html.removeClass=function(node,_402,_403){
try{
if(!_403){
var _404=dojo.html.getClass(node).replace(new RegExp("(^|\\s+)"+_402+"(\\s+|$)"),"$1$2");
}else{
var _404=dojo.html.getClass(node).replace(_402,"");
}
dojo.html.setClass(node,_404);
}
catch(e){
dojo.debug("dojo.html.removeClass() failed",e);
}
return true;
};
dojo.html.replaceClass=function(node,_406,_407){
dojo.html.removeClass(node,_407);
dojo.html.addClass(node,_406);
};
dojo.html.classMatchType={ContainsAll:0,ContainsAny:1,IsOnly:2};
dojo.html.getElementsByClass=function(_408,_409,_40a,_40b,_40c){
_40c=false;
var _40d=dojo.doc();
_409=dojo.byId(_409)||_40d;
var _40e=_408.split(/\s+/g);
var _40f=[];
if(_40b!=1&&_40b!=2){
_40b=0;
}
var _410=new RegExp("(\\s|^)(("+_40e.join(")|(")+"))(\\s|$)");
var _411=_40e.join(" ").length;
var _412=[];
if(!_40c&&_40d.evaluate){
var _413=".//"+(_40a||"*")+"[contains(";
if(_40b!=dojo.html.classMatchType.ContainsAny){
_413+="concat(' ',@class,' '), ' "+_40e.join(" ') and contains(concat(' ',@class,' '), ' ")+" ')";
if(_40b==2){
_413+=" and string-length(@class)="+_411+"]";
}else{
_413+="]";
}
}else{
_413+="concat(' ',@class,' '), ' "+_40e.join(" ') or contains(concat(' ',@class,' '), ' ")+" ')]";
}
var _414=_40d.evaluate(_413,_409,null,XPathResult.ANY_TYPE,null);
var _415=_414.iterateNext();
while(_415){
try{
_412.push(_415);
_415=_414.iterateNext();
}
catch(e){
break;
}
}
return _412;
}else{
if(!_40a){
_40a="*";
}
_412=_409.getElementsByTagName(_40a);
var node,i=0;
outer:
while(node=_412[i++]){
var _418=dojo.html.getClasses(node);
if(_418.length==0){
continue outer;
}
var _419=0;
for(var j=0;j<_418.length;j++){
if(_410.test(_418[j])){
if(_40b==dojo.html.classMatchType.ContainsAny){
_40f.push(node);
continue outer;
}else{
_419++;
}
}else{
if(_40b==dojo.html.classMatchType.IsOnly){
continue outer;
}
}
}
if(_419==_40e.length){
if((_40b==dojo.html.classMatchType.IsOnly)&&(_419==_418.length)){
_40f.push(node);
}else{
if(_40b==dojo.html.classMatchType.ContainsAll){
_40f.push(node);
}
}
}
}
return _40f;
}
};
dojo.html.getElementsByClassName=dojo.html.getElementsByClass;
dojo.html.toCamelCase=function(_41b){
var arr=_41b.split("-"),cc=arr[0];
for(var i=1;i<arr.length;i++){
cc+=arr[i].charAt(0).toUpperCase()+arr[i].substring(1);
}
return cc;
};
dojo.html.toSelectorCase=function(_41f){
return _41f.replace(/([A-Z])/g,"-$1").toLowerCase();
};
if(dojo.render.html.ie){
dojo.html.getComputedStyle=function(node,_421,_422){
node=dojo.byId(node);
if(!node||!node.style){
return _422;
}
return node.currentStyle[dojo.html.toCamelCase(_421)];
};
dojo.html.getComputedStyles=function(node){
return node.currentStyle;
};
}else{
dojo.html.getComputedStyle=function(node,_425,_426){
node=dojo.byId(node);
if(!node||!node.style){
return _426;
}
var s=document.defaultView.getComputedStyle(node,null);
return (s&&s[dojo.html.toCamelCase(_425)])||"";
};
dojo.html.getComputedStyles=function(node){
return document.defaultView.getComputedStyle(node,null);
};
}
dojo.html.getStyleProperty=function(node,_42a){
node=dojo.byId(node);
return (node&&node.style?node.style[dojo.html.toCamelCase(_42a)]:undefined);
};
dojo.html.getStyle=function(node,_42c){
var _42d=dojo.html.getStyleProperty(node,_42c);
return (_42d?_42d:dojo.html.getComputedStyle(node,_42c));
};
dojo.html.setStyle=function(node,_42f,_430){
node=dojo.byId(node);
if(node&&node.style){
var _431=dojo.html.toCamelCase(_42f);
node.style[_431]=_430;
}
};
dojo.html.setStyleText=function(_432,text){
try{
_432.style.cssText=text;
}
catch(e){
_432.setAttribute("style",text);
}
};
dojo.html.copyStyle=function(_434,_435){
if(!_435.style.cssText){
_434.setAttribute("style",_435.getAttribute("style"));
}else{
_434.style.cssText=_435.style.cssText;
}
dojo.html.addClass(_434,dojo.html.getClass(_435));
};
dojo.html.getUnitValue=function(node,_437,_438){
var s=dojo.html.getComputedStyle(node,_437);
if((!s)||((s=="auto")&&(_438))){
return {value:0,units:"px"};
}
var _43a=s.match(/(\-?[\d.]+)([a-z%]*)/i);
if(!_43a){
return dojo.html.getUnitValue.bad;
}
return {value:Number(_43a[1]),units:_43a[2].toLowerCase()};
};
dojo.html.getUnitValue.bad={value:NaN,units:""};
if(dojo.render.html.ie){
dojo.html.toPixelValue=function(_43b,_43c){
if(!_43c){
return 0;
}
if(_43c.slice(-2)=="px"){
return parseFloat(_43c);
}
var _43d=0;
with(_43b){
var _43e=style.left;
var _43f=runtimeStyle.left;
runtimeStyle.left=currentStyle.left;
try{
style.left=_43c||0;
_43d=style.pixelLeft;
style.left=_43e;
runtimeStyle.left=_43f;
}
catch(e){
}
}
return _43d;
};
}else{
dojo.html.toPixelValue=function(_440,_441){
return (_441.slice(-2)=="px"?parseFloat(_441):0);
};
}
dojo.html.getPixelValue=function(node,_443,_444){
return dojo.html.toPixelValue(node,dojo.html.getComputedStyle(node,_443));
};
dojo.html.setPositivePixelValue=function(node,_446,_447){
if(isNaN(_447)){
return false;
}
node.style[_446]=Math.max(0,_447)+"px";
return true;
};
dojo.html.styleSheet=null;
dojo.html.insertCssRule=function(_448,_449,_44a){
if(!dojo.html.styleSheet){
if(document.createStyleSheet){
dojo.html.styleSheet=document.createStyleSheet();
}else{
if(document.styleSheets[0]){
dojo.html.styleSheet=document.styleSheets[0];
}else{
return null;
}
}
}
if(arguments.length<3){
if(dojo.html.styleSheet.cssRules){
_44a=dojo.html.styleSheet.cssRules.length;
}else{
if(dojo.html.styleSheet.rules){
_44a=dojo.html.styleSheet.rules.length;
}else{
return null;
}
}
}
if(dojo.html.styleSheet.insertRule){
var rule=_448+" { "+_449+" }";
return dojo.html.styleSheet.insertRule(rule,_44a);
}else{
if(dojo.html.styleSheet.addRule){
return dojo.html.styleSheet.addRule(_448,_449,_44a);
}else{
return null;
}
}
};
dojo.html.removeCssRule=function(_44c){
if(!dojo.html.styleSheet){
dojo.debug("no stylesheet defined for removing rules");
return false;
}
if(dojo.render.html.ie){
if(!_44c){
_44c=dojo.html.styleSheet.rules.length;
dojo.html.styleSheet.removeRule(_44c);
}
}else{
if(document.styleSheets[0]){
if(!_44c){
_44c=dojo.html.styleSheet.cssRules.length;
}
dojo.html.styleSheet.deleteRule(_44c);
}
}
return true;
};
dojo.html._insertedCssFiles=[];
dojo.html.insertCssFile=function(URI,doc,_44f,_450){
if(!URI){
return;
}
if(!doc){
doc=document;
}
var _451=dojo.hostenv.getText(URI,false,_450);
if(_451===null){
return;
}
_451=dojo.html.fixPathsInCssText(_451,URI);
if(_44f){
var idx=-1,node,ent=dojo.html._insertedCssFiles;
for(var i=0;i<ent.length;i++){
if((ent[i].doc==doc)&&(ent[i].cssText==_451)){
idx=i;
node=ent[i].nodeRef;
break;
}
}
if(node){
var _456=doc.getElementsByTagName("style");
for(var i=0;i<_456.length;i++){
if(_456[i]==node){
return;
}
}
dojo.html._insertedCssFiles.shift(idx,1);
}
}
var _457=dojo.html.insertCssText(_451,doc);
dojo.html._insertedCssFiles.push({"doc":doc,"cssText":_451,"nodeRef":_457});
if(_457&&djConfig.isDebug){
_457.setAttribute("dbgHref",URI);
}
return _457;
};
dojo.html.insertCssText=function(_458,doc,URI){
if(!_458){
return;
}
if(!doc){
doc=document;
}
if(URI){
_458=dojo.html.fixPathsInCssText(_458,URI);
}
var _45b=doc.createElement("style");
_45b.setAttribute("type","text/css");
var head=doc.getElementsByTagName("head")[0];
if(!head){
dojo.debug("No head tag in document, aborting styles");
return;
}else{
head.appendChild(_45b);
}
if(_45b.styleSheet){
var _45d=function(){
try{
_45b.styleSheet.cssText=_458;
}
catch(e){
dojo.debug(e);
}
};
if(_45b.styleSheet.disabled){
setTimeout(_45d,10);
}else{
_45d();
}
}else{
var _45e=doc.createTextNode(_458);
_45b.appendChild(_45e);
}
return _45b;
};
dojo.html.fixPathsInCssText=function(_45f,URI){
if(!_45f||!URI){
return;
}
var _461,str="",url="",_464="[\\t\\s\\w\\(\\)\\/\\.\\\\'\"-:#=&?~]+";
var _465=new RegExp("url\\(\\s*("+_464+")\\s*\\)");
var _466=/(file|https?|ftps?):\/\//;
regexTrim=new RegExp("^[\\s]*(['\"]?)("+_464+")\\1[\\s]*?$");
if(dojo.render.html.ie55||dojo.render.html.ie60){
var _467=new RegExp("AlphaImageLoader\\((.*)src=['\"]("+_464+")['\"]");
while(_461=_467.exec(_45f)){
url=_461[2].replace(regexTrim,"$2");
if(!_466.exec(url)){
url=(new dojo.uri.Uri(URI,url).toString());
}
str+=_45f.substring(0,_461.index)+"AlphaImageLoader("+_461[1]+"src='"+url+"'";
_45f=_45f.substr(_461.index+_461[0].length);
}
_45f=str+_45f;
str="";
}
while(_461=_465.exec(_45f)){
url=_461[1].replace(regexTrim,"$2");
if(!_466.exec(url)){
url=(new dojo.uri.Uri(URI,url).toString());
}
str+=_45f.substring(0,_461.index)+"url("+url+")";
_45f=_45f.substr(_461.index+_461[0].length);
}
return str+_45f;
};
dojo.html.setActiveStyleSheet=function(_468){
var i=0,a,els=dojo.doc().getElementsByTagName("link");
while(a=els[i++]){
if(a.getAttribute("rel").indexOf("style")!=-1&&a.getAttribute("title")){
a.disabled=true;
if(a.getAttribute("title")==_468){
a.disabled=false;
}
}
}
};
dojo.html.getActiveStyleSheet=function(){
var i=0,a,els=dojo.doc().getElementsByTagName("link");
while(a=els[i++]){
if(a.getAttribute("rel").indexOf("style")!=-1&&a.getAttribute("title")&&!a.disabled){
return a.getAttribute("title");
}
}
return null;
};
dojo.html.getPreferredStyleSheet=function(){
var i=0,a,els=dojo.doc().getElementsByTagName("link");
while(a=els[i++]){
if(a.getAttribute("rel").indexOf("style")!=-1&&a.getAttribute("rel").indexOf("alt")==-1&&a.getAttribute("title")){
return a.getAttribute("title");
}
}
return null;
};
dojo.html.applyBrowserClass=function(node){
var drh=dojo.render.html;
var _474={dj_ie:drh.ie,dj_ie55:drh.ie55,dj_ie6:drh.ie60,dj_ie7:drh.ie70,dj_iequirks:drh.ie&&drh.quirks,dj_opera:drh.opera,dj_opera8:drh.opera&&(Math.floor(dojo.render.version)==8),dj_opera9:drh.opera&&(Math.floor(dojo.render.version)==9),dj_khtml:drh.khtml,dj_safari:drh.safari,dj_gecko:drh.mozilla};
for(var p in _474){
if(_474[p]){
dojo.html.addClass(node,p);
}
}
};
dojo.provide("dojo.widget.DomWidget");
dojo.widget._cssFiles={};
dojo.widget._cssStrings={};
dojo.widget._templateCache={};
dojo.widget.defaultStrings={dojoRoot:dojo.hostenv.getBaseScriptUri(),dojoWidgetModuleUri:dojo.uri.moduleUri("dojo.widget"),baseScriptUri:dojo.hostenv.getBaseScriptUri()};
dojo.widget.fillFromTemplateCache=function(obj,_477,_478,_479){
var _47a=_477||obj.templatePath;
var _47b=dojo.widget._templateCache;
if(!_47a&&!obj["widgetType"]){
do{
var _47c="__dummyTemplate__"+dojo.widget._templateCache.dummyCount++;
}while(_47b[_47c]);
obj.widgetType=_47c;
}
var wt=_47a?_47a.toString():obj.widgetType;
var ts=_47b[wt];
if(!ts){
_47b[wt]={"string":null,"node":null};
if(_479){
ts={};
}else{
ts=_47b[wt];
}
}
if((!obj.templateString)&&(!_479)){
obj.templateString=_478||ts["string"];
}
if(obj.templateString){
obj.templateString=this._sanitizeTemplateString(obj.templateString);
}
if((!obj.templateNode)&&(!_479)){
obj.templateNode=ts["node"];
}
if((!obj.templateNode)&&(!obj.templateString)&&(_47a)){
var _47f=this._sanitizeTemplateString(dojo.hostenv.getText(_47a));
obj.templateString=_47f;
if(!_479){
_47b[wt]["string"]=_47f;
}
}
if((!ts["string"])&&(!_479)){
ts.string=obj.templateString;
}
};
dojo.widget._sanitizeTemplateString=function(_480){
if(_480){
_480=_480.replace(/^\s*<\?xml(\s)+version=[\'\"](\d)*.(\d)*[\'\"](\s)*\?>/im,"");
var _481=_480.match(/<body[^>]*>\s*([\s\S]+)\s*<\/body>/im);
if(_481){
_480=_481[1];
}
}else{
_480="";
}
return _480;
};
dojo.widget._templateCache.dummyCount=0;
dojo.widget.attachProperties=["dojoAttachPoint","id"];
dojo.widget.eventAttachProperty="dojoAttachEvent";
dojo.widget.onBuildProperty="dojoOnBuild";
dojo.widget.waiNames=["waiRole","waiState"];
dojo.widget.wai={waiRole:{name:"waiRole","namespace":"http://www.w3.org/TR/xhtml2",alias:"x2",prefix:"wairole:"},waiState:{name:"waiState","namespace":"http://www.w3.org/2005/07/aaa",alias:"aaa",prefix:""},setAttr:function(node,ns,attr,_485){
if(dojo.render.html.ie){
node.setAttribute(this[ns].alias+":"+attr,this[ns].prefix+_485);
}else{
node.setAttributeNS(this[ns]["namespace"],attr,this[ns].prefix+_485);
}
},getAttr:function(node,ns,attr){
if(dojo.render.html.ie){
return node.getAttribute(this[ns].alias+":"+attr);
}else{
return node.getAttributeNS(this[ns]["namespace"],attr);
}
},removeAttr:function(node,ns,attr){
var _48c=true;
if(dojo.render.html.ie){
_48c=node.removeAttribute(this[ns].alias+":"+attr);
}else{
node.removeAttributeNS(this[ns]["namespace"],attr);
}
return _48c;
}};
dojo.widget.attachTemplateNodes=function(_48d,_48e,_48f){
var _490=dojo.dom.ELEMENT_NODE;
function trim(str){
return str.replace(/^\s+|\s+$/g,"");
}
if(!_48d){
_48d=_48e.domNode;
}
if(_48d.nodeType!=_490){
return;
}
var _492=_48d.all||_48d.getElementsByTagName("*");
var _493=_48e;
for(var x=-1;x<_492.length;x++){
var _495=(x==-1)?_48d:_492[x];
var _496=[];
if(!_48e.widgetsInTemplate||!_495.getAttribute("dojoType")){
for(var y=0;y<this.attachProperties.length;y++){
var _498=_495.getAttribute(this.attachProperties[y]);
if(_498){
_496=_498.split(";");
for(var z=0;z<_496.length;z++){
if(dojo.lang.isArray(_48e[_496[z]])){
_48e[_496[z]].push(_495);
}else{
_48e[_496[z]]=_495;
}
}
break;
}
}
var _49a=_495.getAttribute(this.eventAttachProperty);
if(_49a){
var evts=_49a.split(";");
for(var y=0;y<evts.length;y++){
if((!evts[y])||(!evts[y].length)){
continue;
}
var _49c=null;
var tevt=trim(evts[y]);
if(evts[y].indexOf(":")>=0){
var _49e=tevt.split(":");
tevt=trim(_49e[0]);
_49c=trim(_49e[1]);
}
if(!_49c){
_49c=tevt;
}
var tf=function(){
var ntf=new String(_49c);
return function(evt){
if(_493[ntf]){
_493[ntf](dojo.event.browser.fixEvent(evt,this));
}
};
}();
dojo.event.browser.addListener(_495,tevt,tf,false,true);
}
}
for(var y=0;y<_48f.length;y++){
var _4a2=_495.getAttribute(_48f[y]);
if((_4a2)&&(_4a2.length)){
var _49c=null;
var _4a3=_48f[y].substr(4);
_49c=trim(_4a2);
var _4a4=[_49c];
if(_49c.indexOf(";")>=0){
_4a4=dojo.lang.map(_49c.split(";"),trim);
}
for(var z=0;z<_4a4.length;z++){
if(!_4a4[z].length){
continue;
}
var tf=function(){
var ntf=new String(_4a4[z]);
return function(evt){
if(_493[ntf]){
_493[ntf](dojo.event.browser.fixEvent(evt,this));
}
};
}();
dojo.event.browser.addListener(_495,_4a3,tf,false,true);
}
}
}
}
var _4a7=_495.getAttribute(this.templateProperty);
if(_4a7){
_48e[_4a7]=_495;
}
dojo.lang.forEach(dojo.widget.waiNames,function(name){
var wai=dojo.widget.wai[name];
var val=_495.getAttribute(wai.name);
if(val){
if(val.indexOf("-")==-1){
dojo.widget.wai.setAttr(_495,wai.name,"role",val);
}else{
var _4ab=val.split("-");
dojo.widget.wai.setAttr(_495,wai.name,_4ab[0],_4ab[1]);
}
}
},this);
var _4ac=_495.getAttribute(this.onBuildProperty);
if(_4ac){
eval("var node = baseNode; var widget = targetObj; "+_4ac);
}
}
};
dojo.widget.getDojoEventsFromStr=function(str){
var re=/(dojoOn([a-z]+)(\s?))=/gi;
var evts=str?str.match(re)||[]:[];
var ret=[];
var lem={};
for(var x=0;x<evts.length;x++){
if(evts[x].length<1){
continue;
}
var cm=evts[x].replace(/\s/,"");
cm=(cm.slice(0,cm.length-1));
if(!lem[cm]){
lem[cm]=true;
ret.push(cm);
}
}
return ret;
};
dojo.declare("dojo.widget.DomWidget",dojo.widget.Widget,function(){
if((arguments.length>0)&&(typeof arguments[0]=="object")){
this.create(arguments[0]);
}
},{templateNode:null,templateString:null,templateCssString:null,preventClobber:false,domNode:null,containerNode:null,widgetsInTemplate:false,addChild:function(_4b4,_4b5,pos,ref,_4b8){
if(!this.isContainer){
dojo.debug("dojo.widget.DomWidget.addChild() attempted on non-container widget");
return null;
}else{
if(_4b8==undefined){
_4b8=this.children.length;
}
this.addWidgetAsDirectChild(_4b4,_4b5,pos,ref,_4b8);
this.registerChild(_4b4,_4b8);
}
return _4b4;
},addWidgetAsDirectChild:function(_4b9,_4ba,pos,ref,_4bd){
if((!this.containerNode)&&(!_4ba)){
this.containerNode=this.domNode;
}
var cn=(_4ba)?_4ba:this.containerNode;
if(!pos){
pos="after";
}
if(!ref){
if(!cn){
cn=dojo.body();
}
ref=cn.lastChild;
}
if(!_4bd){
_4bd=0;
}
_4b9.domNode.setAttribute("dojoinsertionindex",_4bd);
if(!ref){
cn.appendChild(_4b9.domNode);
}else{
if(pos=="insertAtIndex"){
dojo.dom.insertAtIndex(_4b9.domNode,ref.parentNode,_4bd);
}else{
if((pos=="after")&&(ref===cn.lastChild)){
cn.appendChild(_4b9.domNode);
}else{
dojo.dom.insertAtPosition(_4b9.domNode,cn,pos);
}
}
}
},registerChild:function(_4bf,_4c0){
_4bf.dojoInsertionIndex=_4c0;
var idx=-1;
for(var i=0;i<this.children.length;i++){
if(this.children[i].dojoInsertionIndex<=_4c0){
idx=i;
}
}
this.children.splice(idx+1,0,_4bf);
_4bf.parent=this;
_4bf.addedTo(this,idx+1);
delete dojo.widget.manager.topWidgets[_4bf.widgetId];
},removeChild:function(_4c3){
dojo.dom.removeNode(_4c3.domNode);
return dojo.widget.DomWidget.superclass.removeChild.call(this,_4c3);
},getFragNodeRef:function(frag){
if(!frag){
return null;
}
if(!frag[this.getNamespacedType()]){
dojo.raise("Error: no frag for widget type "+this.getNamespacedType()+", id "+this.widgetId+" (maybe a widget has set it's type incorrectly)");
}
return frag[this.getNamespacedType()]["nodeRef"];
},postInitialize:function(args,frag,_4c7){
var _4c8=this.getFragNodeRef(frag);
if(_4c7&&(_4c7.snarfChildDomOutput||!_4c8)){
_4c7.addWidgetAsDirectChild(this,"","insertAtIndex","",args["dojoinsertionindex"],_4c8);
}else{
if(_4c8){
if(this.domNode&&(this.domNode!==_4c8)){
this._sourceNodeRef=dojo.dom.replaceNode(_4c8,this.domNode);
}
}
}
if(_4c7){
_4c7.registerChild(this,args.dojoinsertionindex);
}else{
dojo.widget.manager.topWidgets[this.widgetId]=this;
}
if(this.widgetsInTemplate){
var _4c9=new dojo.xml.Parse();
var _4ca;
var _4cb=this.domNode.getElementsByTagName("*");
for(var i=0;i<_4cb.length;i++){
if(_4cb[i].getAttribute("dojoAttachPoint")=="subContainerWidget"){
_4ca=_4cb[i];
}
if(_4cb[i].getAttribute("dojoType")){
_4cb[i].setAttribute("isSubWidget",true);
}
}
if(this.isContainer&&!this.containerNode){
if(_4ca){
var src=this.getFragNodeRef(frag);
if(src){
dojo.dom.moveChildren(src,_4ca);
frag["dojoDontFollow"]=true;
}
}else{
dojo.debug("No subContainerWidget node can be found in template file for widget "+this);
}
}
var _4ce=_4c9.parseElement(this.domNode,null,true);
dojo.widget.getParser().createSubComponents(_4ce,this);
var _4cf=[];
var _4d0=[this];
var w;
while((w=_4d0.pop())){
for(var i=0;i<w.children.length;i++){
var _4d2=w.children[i];
if(_4d2._processedSubWidgets||!_4d2.extraArgs["issubwidget"]){
continue;
}
_4cf.push(_4d2);
if(_4d2.isContainer){
_4d0.push(_4d2);
}
}
}
for(var i=0;i<_4cf.length;i++){
var _4d3=_4cf[i];
if(_4d3._processedSubWidgets){
dojo.debug("This should not happen: widget._processedSubWidgets is already true!");
return;
}
_4d3._processedSubWidgets=true;
if(_4d3.extraArgs["dojoattachevent"]){
var evts=_4d3.extraArgs["dojoattachevent"].split(";");
for(var j=0;j<evts.length;j++){
var _4d6=null;
var tevt=dojo.string.trim(evts[j]);
if(tevt.indexOf(":")>=0){
var _4d8=tevt.split(":");
tevt=dojo.string.trim(_4d8[0]);
_4d6=dojo.string.trim(_4d8[1]);
}
if(!_4d6){
_4d6=tevt;
}
if(dojo.lang.isFunction(_4d3[tevt])){
dojo.event.kwConnect({srcObj:_4d3,srcFunc:tevt,targetObj:this,targetFunc:_4d6});
}else{
alert(tevt+" is not a function in widget "+_4d3);
}
}
}
if(_4d3.extraArgs["dojoattachpoint"]){
this[_4d3.extraArgs["dojoattachpoint"]]=_4d3;
}
}
}
if(this.isContainer&&!frag["dojoDontFollow"]){
dojo.widget.getParser().createSubComponents(frag,this);
}
},buildRendering:function(args,frag){
var ts=dojo.widget._templateCache[this.widgetType];
if(args["templatecsspath"]){
args["templateCssPath"]=args["templatecsspath"];
}
var _4dc=args["templateCssPath"]||this.templateCssPath;
if(_4dc&&!dojo.widget._cssFiles[_4dc.toString()]){
if((!this.templateCssString)&&(_4dc)){
this.templateCssString=dojo.hostenv.getText(_4dc);
this.templateCssPath=null;
}
dojo.widget._cssFiles[_4dc.toString()]=true;
}
if((this["templateCssString"])&&(!dojo.widget._cssStrings[this.templateCssString])){
dojo.html.insertCssText(this.templateCssString,null,_4dc);
dojo.widget._cssStrings[this.templateCssString]=true;
}
if((!this.preventClobber)&&((this.templatePath)||(this.templateNode)||((this["templateString"])&&(this.templateString.length))||((typeof ts!="undefined")&&((ts["string"])||(ts["node"]))))){
this.buildFromTemplate(args,frag);
}else{
this.domNode=this.getFragNodeRef(frag);
}
this.fillInTemplate(args,frag);
},buildFromTemplate:function(args,frag){
var _4df=false;
if(args["templatepath"]){
args["templatePath"]=args["templatepath"];
}
dojo.widget.fillFromTemplateCache(this,args["templatePath"],null,_4df);
var ts=dojo.widget._templateCache[this.templatePath?this.templatePath.toString():this.widgetType];
if((ts)&&(!_4df)){
if(!this.templateString.length){
this.templateString=ts["string"];
}
if(!this.templateNode){
this.templateNode=ts["node"];
}
}
var _4e1=false;
var node=null;
var tstr=this.templateString;
if((!this.templateNode)&&(this.templateString)){
_4e1=this.templateString.match(/\$\{([^\}]+)\}/g);
if(_4e1){
var hash=this.strings||{};
for(var key in dojo.widget.defaultStrings){
if(dojo.lang.isUndefined(hash[key])){
hash[key]=dojo.widget.defaultStrings[key];
}
}
for(var i=0;i<_4e1.length;i++){
var key=_4e1[i];
key=key.substring(2,key.length-1);
var kval=(key.substring(0,5)=="this.")?dojo.lang.getObjPathValue(key.substring(5),this):hash[key];
var _4e8;
if((kval)||(dojo.lang.isString(kval))){
_4e8=new String((dojo.lang.isFunction(kval))?kval.call(this,key,this.templateString):kval);
while(_4e8.indexOf("\"")>-1){
_4e8=_4e8.replace("\"","&quot;");
}
tstr=tstr.replace(_4e1[i],_4e8);
}
}
}else{
this.templateNode=this.createNodesFromText(this.templateString,true)[0];
if(!_4df){
ts.node=this.templateNode;
}
}
}
if((!this.templateNode)&&(!_4e1)){
dojo.debug("DomWidget.buildFromTemplate: could not create template");
return false;
}else{
if(!_4e1){
node=this.templateNode.cloneNode(true);
if(!node){
return false;
}
}else{
node=this.createNodesFromText(tstr,true)[0];
}
}
this.domNode=node;
this.attachTemplateNodes();
if(this.isContainer&&this.containerNode){
var src=this.getFragNodeRef(frag);
if(src){
dojo.dom.moveChildren(src,this.containerNode);
}
}
},attachTemplateNodes:function(_4ea,_4eb){
if(!_4ea){
_4ea=this.domNode;
}
if(!_4eb){
_4eb=this;
}
return dojo.widget.attachTemplateNodes(_4ea,_4eb,dojo.widget.getDojoEventsFromStr(this.templateString));
},fillInTemplate:function(){
},destroyRendering:function(){
try{
dojo.dom.destroyNode(this.domNode);
delete this.domNode;
}
catch(e){
}
if(this._sourceNodeRef){
try{
dojo.dom.destroyNode(this._sourceNodeRef);
}
catch(e){
}
}
},createNodesFromText:function(){
dojo.unimplemented("dojo.widget.DomWidget.createNodesFromText");
}});
dojo.provide("dojo.html.display");
dojo.html._toggle=function(node,_4ed,_4ee){
node=dojo.byId(node);
_4ee(node,!_4ed(node));
return _4ed(node);
};
dojo.html.show=function(node){
node=dojo.byId(node);
if(dojo.html.getStyleProperty(node,"display")=="none"){
dojo.html.setStyle(node,"display",(node.dojoDisplayCache||""));
node.dojoDisplayCache=undefined;
}
};
dojo.html.hide=function(node){
node=dojo.byId(node);
if(typeof node["dojoDisplayCache"]=="undefined"){
var d=dojo.html.getStyleProperty(node,"display");
if(d!="none"){
node.dojoDisplayCache=d;
}
}
dojo.html.setStyle(node,"display","none");
};
dojo.html.setShowing=function(node,_4f3){
dojo.html[(_4f3?"show":"hide")](node);
};
dojo.html.isShowing=function(node){
return (dojo.html.getStyleProperty(node,"display")!="none");
};
dojo.html.toggleShowing=function(node){
return dojo.html._toggle(node,dojo.html.isShowing,dojo.html.setShowing);
};
dojo.html.displayMap={tr:"",td:"",th:"",img:"inline",span:"inline",input:"inline",button:"inline"};
dojo.html.suggestDisplayByTagName=function(node){
node=dojo.byId(node);
if(node&&node.tagName){
var tag=node.tagName.toLowerCase();
return (tag in dojo.html.displayMap?dojo.html.displayMap[tag]:"block");
}
};
dojo.html.setDisplay=function(node,_4f9){
dojo.html.setStyle(node,"display",((_4f9 instanceof String||typeof _4f9=="string")?_4f9:(_4f9?dojo.html.suggestDisplayByTagName(node):"none")));
};
dojo.html.isDisplayed=function(node){
return (dojo.html.getComputedStyle(node,"display")!="none");
};
dojo.html.toggleDisplay=function(node){
return dojo.html._toggle(node,dojo.html.isDisplayed,dojo.html.setDisplay);
};
dojo.html.setVisibility=function(node,_4fd){
dojo.html.setStyle(node,"visibility",((_4fd instanceof String||typeof _4fd=="string")?_4fd:(_4fd?"visible":"hidden")));
};
dojo.html.isVisible=function(node){
return (dojo.html.getComputedStyle(node,"visibility")!="hidden");
};
dojo.html.toggleVisibility=function(node){
return dojo.html._toggle(node,dojo.html.isVisible,dojo.html.setVisibility);
};
dojo.html.setOpacity=function(node,_501,_502){
node=dojo.byId(node);
var h=dojo.render.html;
if(!_502){
if(_501>=1){
if(h.ie){
dojo.html.clearOpacity(node);
return;
}else{
_501=0.999999;
}
}else{
if(_501<0){
_501=0;
}
}
}
if(h.ie){
if(node.nodeName.toLowerCase()=="tr"){
var tds=node.getElementsByTagName("td");
for(var x=0;x<tds.length;x++){
tds[x].style.filter="Alpha(Opacity="+_501*100+")";
}
}
node.style.filter="Alpha(Opacity="+_501*100+")";
}else{
if(h.moz){
node.style.opacity=_501;
node.style.MozOpacity=_501;
}else{
if(h.safari){
node.style.opacity=_501;
node.style.KhtmlOpacity=_501;
}else{
node.style.opacity=_501;
}
}
}
};
dojo.html.clearOpacity=function(node){
node=dojo.byId(node);
var ns=node.style;
var h=dojo.render.html;
if(h.ie){
try{
if(node.filters&&node.filters.alpha){
ns.filter="";
}
}
catch(e){
}
}else{
if(h.moz){
ns.opacity=1;
ns.MozOpacity=1;
}else{
if(h.safari){
ns.opacity=1;
ns.KhtmlOpacity=1;
}else{
ns.opacity=1;
}
}
}
};
dojo.html.getOpacity=function(node){
node=dojo.byId(node);
var h=dojo.render.html;
if(h.ie){
var opac=(node.filters&&node.filters.alpha&&typeof node.filters.alpha.opacity=="number"?node.filters.alpha.opacity:100)/100;
}else{
var opac=node.style.opacity||node.style.MozOpacity||node.style.KhtmlOpacity||1;
}
return opac>=0.999999?1:Number(opac);
};
dojo.provide("dojo.html.layout");
dojo.html.sumAncestorProperties=function(node,prop){
node=dojo.byId(node);
if(!node){
return 0;
}
var _50e=0;
while(node){
if(dojo.html.getComputedStyle(node,"position")=="fixed"){
return 0;
}
var val=node[prop];
if(val){
_50e+=val-0;
if(node==dojo.body()){
break;
}
}
node=node.parentNode;
}
return _50e;
};
dojo.html.setStyleAttributes=function(node,_511){
node=dojo.byId(node);
var _512=_511.replace(/(;)?\s*$/,"").split(";");
for(var i=0;i<_512.length;i++){
var _514=_512[i].split(":");
var name=_514[0].replace(/\s*$/,"").replace(/^\s*/,"").toLowerCase();
var _516=_514[1].replace(/\s*$/,"").replace(/^\s*/,"");
switch(name){
case "opacity":
dojo.html.setOpacity(node,_516);
break;
case "content-height":
dojo.html.setContentBox(node,{height:_516});
break;
case "content-width":
dojo.html.setContentBox(node,{width:_516});
break;
case "outer-height":
dojo.html.setMarginBox(node,{height:_516});
break;
case "outer-width":
dojo.html.setMarginBox(node,{width:_516});
break;
default:
node.style[dojo.html.toCamelCase(name)]=_516;
}
}
};
dojo.html.boxSizing={MARGIN_BOX:"margin-box",BORDER_BOX:"border-box",PADDING_BOX:"padding-box",CONTENT_BOX:"content-box"};
dojo.html.getAbsolutePosition=dojo.html.abs=function(node,_518,_519){
node=dojo.byId(node,node.ownerDocument);
var ret={x:0,y:0};
var bs=dojo.html.boxSizing;
if(!_519){
_519=bs.CONTENT_BOX;
}
var _51c=2;
var _51d;
switch(_519){
case bs.MARGIN_BOX:
_51d=3;
break;
case bs.BORDER_BOX:
_51d=2;
break;
case bs.PADDING_BOX:
default:
_51d=1;
break;
case bs.CONTENT_BOX:
_51d=0;
break;
}
var h=dojo.render.html;
var db=document["body"]||document["documentElement"];
if(h.ie){
with(node.getBoundingClientRect()){
ret.x=left-2;
ret.y=top-2;
}
}else{
if(document.getBoxObjectFor){
_51c=1;
try{
var bo=document.getBoxObjectFor(node);
ret.x=bo.x-dojo.html.sumAncestorProperties(node,"scrollLeft");
ret.y=bo.y-dojo.html.sumAncestorProperties(node,"scrollTop");
}
catch(e){
}
}else{
if(node["offsetParent"]){
var _521;
if((h.safari)&&(node.style.getPropertyValue("position")=="absolute")&&(node.parentNode==db)){
_521=db;
}else{
_521=db.parentNode;
}
if(node.parentNode!=db){
var nd=node;
if(dojo.render.html.opera){
nd=db;
}
ret.x-=dojo.html.sumAncestorProperties(nd,"scrollLeft");
ret.y-=dojo.html.sumAncestorProperties(nd,"scrollTop");
}
var _523=node;
do{
var n=_523["offsetLeft"];
if(!h.opera||n>0){
ret.x+=isNaN(n)?0:n;
}
var m=_523["offsetTop"];
ret.y+=isNaN(m)?0:m;
_523=_523.offsetParent;
}while((_523!=_521)&&(_523!=null));
}else{
if(node["x"]&&node["y"]){
ret.x+=isNaN(node.x)?0:node.x;
ret.y+=isNaN(node.y)?0:node.y;
}
}
}
}
if(_518){
var _526=dojo.html.getScroll();
ret.y+=_526.top;
ret.x+=_526.left;
}
var _527=[dojo.html.getPaddingExtent,dojo.html.getBorderExtent,dojo.html.getMarginExtent];
if(_51c>_51d){
for(var i=_51d;i<_51c;++i){
ret.y+=_527[i](node,"top");
ret.x+=_527[i](node,"left");
}
}else{
if(_51c<_51d){
for(var i=_51d;i>_51c;--i){
ret.y-=_527[i-1](node,"top");
ret.x-=_527[i-1](node,"left");
}
}
}
ret.top=ret.y;
ret.left=ret.x;
return ret;
};
dojo.html.isPositionAbsolute=function(node){
return (dojo.html.getComputedStyle(node,"position")=="absolute");
};
dojo.html._sumPixelValues=function(node,_52b,_52c){
var _52d=0;
for(var x=0;x<_52b.length;x++){
_52d+=dojo.html.getPixelValue(node,_52b[x],_52c);
}
return _52d;
};
dojo.html.getMargin=function(node){
return {width:dojo.html._sumPixelValues(node,["margin-left","margin-right"],(dojo.html.getComputedStyle(node,"position")=="absolute")),height:dojo.html._sumPixelValues(node,["margin-top","margin-bottom"],(dojo.html.getComputedStyle(node,"position")=="absolute"))};
};
dojo.html.getBorder=function(node){
return {width:dojo.html.getBorderExtent(node,"left")+dojo.html.getBorderExtent(node,"right"),height:dojo.html.getBorderExtent(node,"top")+dojo.html.getBorderExtent(node,"bottom")};
};
dojo.html.getBorderExtent=function(node,side){
return (dojo.html.getStyle(node,"border-"+side+"-style")=="none"?0:dojo.html.getPixelValue(node,"border-"+side+"-width"));
};
dojo.html.getMarginExtent=function(node,side){
return dojo.html._sumPixelValues(node,["margin-"+side],dojo.html.isPositionAbsolute(node));
};
dojo.html.getPaddingExtent=function(node,side){
return dojo.html._sumPixelValues(node,["padding-"+side],true);
};
dojo.html.getPadding=function(node){
return {width:dojo.html._sumPixelValues(node,["padding-left","padding-right"],true),height:dojo.html._sumPixelValues(node,["padding-top","padding-bottom"],true)};
};
dojo.html.getPadBorder=function(node){
var pad=dojo.html.getPadding(node);
var _53a=dojo.html.getBorder(node);
return {width:pad.width+_53a.width,height:pad.height+_53a.height};
};
dojo.html.getBoxSizing=function(node){
var h=dojo.render.html;
var bs=dojo.html.boxSizing;
if(((h.ie)||(h.opera))&&node.nodeName!="IMG"){
var cm=document["compatMode"];
if((cm=="BackCompat")||(cm=="QuirksMode")){
return bs.BORDER_BOX;
}else{
return bs.CONTENT_BOX;
}
}else{
if(arguments.length==0){
node=document.documentElement;
}
var _53f=dojo.html.getStyle(node,"-moz-box-sizing");
if(!_53f){
_53f=dojo.html.getStyle(node,"box-sizing");
}
return (_53f?_53f:bs.CONTENT_BOX);
}
};
dojo.html.isBorderBox=function(node){
return (dojo.html.getBoxSizing(node)==dojo.html.boxSizing.BORDER_BOX);
};
dojo.html.getBorderBox=function(node){
node=dojo.byId(node);
return {width:node.offsetWidth,height:node.offsetHeight};
};
dojo.html.getPaddingBox=function(node){
var box=dojo.html.getBorderBox(node);
var _544=dojo.html.getBorder(node);
return {width:box.width-_544.width,height:box.height-_544.height};
};
dojo.html.getContentBox=function(node){
node=dojo.byId(node);
var _546=dojo.html.getPadBorder(node);
return {width:node.offsetWidth-_546.width,height:node.offsetHeight-_546.height};
};
dojo.html.setContentBox=function(node,args){
node=dojo.byId(node);
var _549=0;
var _54a=0;
var isbb=dojo.html.isBorderBox(node);
var _54c=(isbb?dojo.html.getPadBorder(node):{width:0,height:0});
var ret={};
if(typeof args.width!="undefined"){
_549=args.width+_54c.width;
ret.width=dojo.html.setPositivePixelValue(node,"width",_549);
}
if(typeof args.height!="undefined"){
_54a=args.height+_54c.height;
ret.height=dojo.html.setPositivePixelValue(node,"height",_54a);
}
return ret;
};
dojo.html.getMarginBox=function(node){
var _54f=dojo.html.getBorderBox(node);
var _550=dojo.html.getMargin(node);
return {width:_54f.width+_550.width,height:_54f.height+_550.height};
};
dojo.html.setMarginBox=function(node,args){
node=dojo.byId(node);
var _553=0;
var _554=0;
var isbb=dojo.html.isBorderBox(node);
var _556=(!isbb?dojo.html.getPadBorder(node):{width:0,height:0});
var _557=dojo.html.getMargin(node);
var ret={};
if(typeof args.width!="undefined"){
_553=args.width-_556.width;
_553-=_557.width;
ret.width=dojo.html.setPositivePixelValue(node,"width",_553);
}
if(typeof args.height!="undefined"){
_554=args.height-_556.height;
_554-=_557.height;
ret.height=dojo.html.setPositivePixelValue(node,"height",_554);
}
return ret;
};
dojo.html.getElementBox=function(node,type){
var bs=dojo.html.boxSizing;
switch(type){
case bs.MARGIN_BOX:
return dojo.html.getMarginBox(node);
case bs.BORDER_BOX:
return dojo.html.getBorderBox(node);
case bs.PADDING_BOX:
return dojo.html.getPaddingBox(node);
case bs.CONTENT_BOX:
default:
return dojo.html.getContentBox(node);
}
};
dojo.html.toCoordinateObject=dojo.html.toCoordinateArray=function(_55c,_55d,_55e){
if(_55c instanceof Array||typeof _55c=="array"){
dojo.deprecated("dojo.html.toCoordinateArray","use dojo.html.toCoordinateObject({left: , top: , width: , height: }) instead","0.5");
while(_55c.length<4){
_55c.push(0);
}
while(_55c.length>4){
_55c.pop();
}
var ret={left:_55c[0],top:_55c[1],width:_55c[2],height:_55c[3]};
}else{
if(!_55c.nodeType&&!(_55c instanceof String||typeof _55c=="string")&&("width" in _55c||"height" in _55c||"left" in _55c||"x" in _55c||"top" in _55c||"y" in _55c)){
var ret={left:_55c.left||_55c.x||0,top:_55c.top||_55c.y||0,width:_55c.width||0,height:_55c.height||0};
}else{
var node=dojo.byId(_55c);
var pos=dojo.html.abs(node,_55d,_55e);
var _562=dojo.html.getMarginBox(node);
var ret={left:pos.left,top:pos.top,width:_562.width,height:_562.height};
}
}
ret.x=ret.left;
ret.y=ret.top;
return ret;
};
dojo.html.setMarginBoxWidth=dojo.html.setOuterWidth=function(node,_564){
return dojo.html._callDeprecated("setMarginBoxWidth","setMarginBox",arguments,"width");
};
dojo.html.setMarginBoxHeight=dojo.html.setOuterHeight=function(){
return dojo.html._callDeprecated("setMarginBoxHeight","setMarginBox",arguments,"height");
};
dojo.html.getMarginBoxWidth=dojo.html.getOuterWidth=function(){
return dojo.html._callDeprecated("getMarginBoxWidth","getMarginBox",arguments,null,"width");
};
dojo.html.getMarginBoxHeight=dojo.html.getOuterHeight=function(){
return dojo.html._callDeprecated("getMarginBoxHeight","getMarginBox",arguments,null,"height");
};
dojo.html.getTotalOffset=function(node,type,_567){
return dojo.html._callDeprecated("getTotalOffset","getAbsolutePosition",arguments,null,type);
};
dojo.html.getAbsoluteX=function(node,_569){
return dojo.html._callDeprecated("getAbsoluteX","getAbsolutePosition",arguments,null,"x");
};
dojo.html.getAbsoluteY=function(node,_56b){
return dojo.html._callDeprecated("getAbsoluteY","getAbsolutePosition",arguments,null,"y");
};
dojo.html.totalOffsetLeft=function(node,_56d){
return dojo.html._callDeprecated("totalOffsetLeft","getAbsolutePosition",arguments,null,"left");
};
dojo.html.totalOffsetTop=function(node,_56f){
return dojo.html._callDeprecated("totalOffsetTop","getAbsolutePosition",arguments,null,"top");
};
dojo.html.getMarginWidth=function(node){
return dojo.html._callDeprecated("getMarginWidth","getMargin",arguments,null,"width");
};
dojo.html.getMarginHeight=function(node){
return dojo.html._callDeprecated("getMarginHeight","getMargin",arguments,null,"height");
};
dojo.html.getBorderWidth=function(node){
return dojo.html._callDeprecated("getBorderWidth","getBorder",arguments,null,"width");
};
dojo.html.getBorderHeight=function(node){
return dojo.html._callDeprecated("getBorderHeight","getBorder",arguments,null,"height");
};
dojo.html.getPaddingWidth=function(node){
return dojo.html._callDeprecated("getPaddingWidth","getPadding",arguments,null,"width");
};
dojo.html.getPaddingHeight=function(node){
return dojo.html._callDeprecated("getPaddingHeight","getPadding",arguments,null,"height");
};
dojo.html.getPadBorderWidth=function(node){
return dojo.html._callDeprecated("getPadBorderWidth","getPadBorder",arguments,null,"width");
};
dojo.html.getPadBorderHeight=function(node){
return dojo.html._callDeprecated("getPadBorderHeight","getPadBorder",arguments,null,"height");
};
dojo.html.getBorderBoxWidth=dojo.html.getInnerWidth=function(){
return dojo.html._callDeprecated("getBorderBoxWidth","getBorderBox",arguments,null,"width");
};
dojo.html.getBorderBoxHeight=dojo.html.getInnerHeight=function(){
return dojo.html._callDeprecated("getBorderBoxHeight","getBorderBox",arguments,null,"height");
};
dojo.html.getContentBoxWidth=dojo.html.getContentWidth=function(){
return dojo.html._callDeprecated("getContentBoxWidth","getContentBox",arguments,null,"width");
};
dojo.html.getContentBoxHeight=dojo.html.getContentHeight=function(){
return dojo.html._callDeprecated("getContentBoxHeight","getContentBox",arguments,null,"height");
};
dojo.html.setContentBoxWidth=dojo.html.setContentWidth=function(node,_579){
return dojo.html._callDeprecated("setContentBoxWidth","setContentBox",arguments,"width");
};
dojo.html.setContentBoxHeight=dojo.html.setContentHeight=function(node,_57b){
return dojo.html._callDeprecated("setContentBoxHeight","setContentBox",arguments,"height");
};
dojo.provide("dojo.html.util");
dojo.html.getElementWindow=function(_57c){
return dojo.html.getDocumentWindow(_57c.ownerDocument);
};
dojo.html.getDocumentWindow=function(doc){
if(dojo.render.html.safari&&!doc._parentWindow){
var fix=function(win){
win.document._parentWindow=win;
for(var i=0;i<win.frames.length;i++){
fix(win.frames[i]);
}
};
fix(window.top);
}
if(dojo.render.html.ie&&window!==document.parentWindow&&!doc._parentWindow){
doc.parentWindow.execScript("document._parentWindow = window;","Javascript");
var win=doc._parentWindow;
doc._parentWindow=null;
return win;
}
return doc._parentWindow||doc.parentWindow||doc.defaultView;
};
dojo.html.gravity=function(node,e){
node=dojo.byId(node);
var _584=dojo.html.getCursorPosition(e);
with(dojo.html){
var _585=getAbsolutePosition(node,true);
var bb=getBorderBox(node);
var _587=_585.x+(bb.width/2);
var _588=_585.y+(bb.height/2);
}
with(dojo.html.gravity){
return ((_584.x<_587?WEST:EAST)|(_584.y<_588?NORTH:SOUTH));
}
};
dojo.html.gravity.NORTH=1;
dojo.html.gravity.SOUTH=1<<1;
dojo.html.gravity.EAST=1<<2;
dojo.html.gravity.WEST=1<<3;
dojo.html.overElement=function(_589,e){
_589=dojo.byId(_589);
var _58b=dojo.html.getCursorPosition(e);
var bb=dojo.html.getBorderBox(_589);
var _58d=dojo.html.getAbsolutePosition(_589,true,dojo.html.boxSizing.BORDER_BOX);
var top=_58d.y;
var _58f=top+bb.height;
var left=_58d.x;
var _591=left+bb.width;
return (_58b.x>=left&&_58b.x<=_591&&_58b.y>=top&&_58b.y<=_58f);
};
dojo.html.renderedTextContent=function(node){
node=dojo.byId(node);
var _593="";
if(node==null){
return _593;
}
for(var i=0;i<node.childNodes.length;i++){
switch(node.childNodes[i].nodeType){
case 1:
case 5:
var _595="unknown";
try{
_595=dojo.html.getStyle(node.childNodes[i],"display");
}
catch(E){
}
switch(_595){
case "block":
case "list-item":
case "run-in":
case "table":
case "table-row-group":
case "table-header-group":
case "table-footer-group":
case "table-row":
case "table-column-group":
case "table-column":
case "table-cell":
case "table-caption":
_593+="\n";
_593+=dojo.html.renderedTextContent(node.childNodes[i]);
_593+="\n";
break;
case "none":
break;
default:
if(node.childNodes[i].tagName&&node.childNodes[i].tagName.toLowerCase()=="br"){
_593+="\n";
}else{
_593+=dojo.html.renderedTextContent(node.childNodes[i]);
}
break;
}
break;
case 3:
case 2:
case 4:
var text=node.childNodes[i].nodeValue;
var _597="unknown";
try{
_597=dojo.html.getStyle(node,"text-transform");
}
catch(E){
}
switch(_597){
case "capitalize":
var _598=text.split(" ");
for(var i=0;i<_598.length;i++){
_598[i]=_598[i].charAt(0).toUpperCase()+_598[i].substring(1);
}
text=_598.join(" ");
break;
case "uppercase":
text=text.toUpperCase();
break;
case "lowercase":
text=text.toLowerCase();
break;
default:
break;
}
switch(_597){
case "nowrap":
break;
case "pre-wrap":
break;
case "pre-line":
break;
case "pre":
break;
default:
text=text.replace(/\s+/," ");
if(/\s$/.test(_593)){
text.replace(/^\s/,"");
}
break;
}
_593+=text;
break;
default:
break;
}
}
return _593;
};
dojo.html.createNodesFromText=function(txt,trim){
if(trim){
txt=txt.replace(/^\s+|\s+$/g,"");
}
var tn=dojo.doc().createElement("div");
tn.style.visibility="hidden";
dojo.body().appendChild(tn);
var _59c="none";
if((/^<t[dh][\s\r\n>]/i).test(txt.replace(/^\s+/))){
txt="<table><tbody><tr>"+txt+"</tr></tbody></table>";
_59c="cell";
}else{
if((/^<tr[\s\r\n>]/i).test(txt.replace(/^\s+/))){
txt="<table><tbody>"+txt+"</tbody></table>";
_59c="row";
}else{
if((/^<(thead|tbody|tfoot)[\s\r\n>]/i).test(txt.replace(/^\s+/))){
txt="<table>"+txt+"</table>";
_59c="section";
}
}
}
tn.innerHTML=txt;
if(tn["normalize"]){
tn.normalize();
}
var _59d=null;
switch(_59c){
case "cell":
_59d=tn.getElementsByTagName("tr")[0];
break;
case "row":
_59d=tn.getElementsByTagName("tbody")[0];
break;
case "section":
_59d=tn.getElementsByTagName("table")[0];
break;
default:
_59d=tn;
break;
}
var _59e=[];
for(var x=0;x<_59d.childNodes.length;x++){
_59e.push(_59d.childNodes[x].cloneNode(true));
}
tn.style.display="none";
dojo.html.destroyNode(tn);
return _59e;
};
dojo.html.placeOnScreen=function(node,_5a1,_5a2,_5a3,_5a4,_5a5,_5a6){
if(_5a1 instanceof Array||typeof _5a1=="array"){
_5a6=_5a5;
_5a5=_5a4;
_5a4=_5a3;
_5a3=_5a2;
_5a2=_5a1[1];
_5a1=_5a1[0];
}
if(_5a5 instanceof String||typeof _5a5=="string"){
_5a5=_5a5.split(",");
}
if(!isNaN(_5a3)){
_5a3=[Number(_5a3),Number(_5a3)];
}else{
if(!(_5a3 instanceof Array||typeof _5a3=="array")){
_5a3=[0,0];
}
}
var _5a7=dojo.html.getScroll().offset;
var view=dojo.html.getViewport();
node=dojo.byId(node);
var _5a9=node.style.display;
node.style.display="";
var bb=dojo.html.getBorderBox(node);
var w=bb.width;
var h=bb.height;
node.style.display=_5a9;
if(!(_5a5 instanceof Array||typeof _5a5=="array")){
_5a5=["TL"];
}
var _5ad,_5ae,_5af=Infinity,_5b0;
for(var _5b1=0;_5b1<_5a5.length;++_5b1){
var _5b2=_5a5[_5b1];
var _5b3=true;
var tryX=_5a1-(_5b2.charAt(1)=="L"?0:w)+_5a3[0]*(_5b2.charAt(1)=="L"?1:-1);
var tryY=_5a2-(_5b2.charAt(0)=="T"?0:h)+_5a3[1]*(_5b2.charAt(0)=="T"?1:-1);
if(_5a4){
tryX-=_5a7.x;
tryY-=_5a7.y;
}
if(tryX<0){
tryX=0;
_5b3=false;
}
if(tryY<0){
tryY=0;
_5b3=false;
}
var x=tryX+w;
if(x>view.width){
x=view.width-w;
_5b3=false;
}else{
x=tryX;
}
x=Math.max(_5a3[0],x)+_5a7.x;
var y=tryY+h;
if(y>view.height){
y=view.height-h;
_5b3=false;
}else{
y=tryY;
}
y=Math.max(_5a3[1],y)+_5a7.y;
if(_5b3){
_5ad=x;
_5ae=y;
_5af=0;
_5b0=_5b2;
break;
}else{
var dist=Math.pow(x-tryX-_5a7.x,2)+Math.pow(y-tryY-_5a7.y,2);
if(_5af>dist){
_5af=dist;
_5ad=x;
_5ae=y;
_5b0=_5b2;
}
}
}
if(!_5a6){
node.style.left=_5ad+"px";
node.style.top=_5ae+"px";
}
return {left:_5ad,top:_5ae,x:_5ad,y:_5ae,dist:_5af,corner:_5b0};
};
dojo.html.placeOnScreenPoint=function(node,_5ba,_5bb,_5bc,_5bd){
dojo.deprecated("dojo.html.placeOnScreenPoint","use dojo.html.placeOnScreen() instead","0.5");
return dojo.html.placeOnScreen(node,_5ba,_5bb,_5bc,_5bd,["TL","TR","BL","BR"]);
};
dojo.html.placeOnScreenAroundElement=function(node,_5bf,_5c0,_5c1,_5c2,_5c3){
var best,_5c5=Infinity;
_5bf=dojo.byId(_5bf);
var _5c6=_5bf.style.display;
_5bf.style.display="";
var mb=dojo.html.getElementBox(_5bf,_5c1);
var _5c8=mb.width;
var _5c9=mb.height;
var _5ca=dojo.html.getAbsolutePosition(_5bf,true,_5c1);
_5bf.style.display=_5c6;
for(var _5cb in _5c2){
var pos,_5cd,_5ce;
var _5cf=_5c2[_5cb];
_5cd=_5ca.x+(_5cb.charAt(1)=="L"?0:_5c8);
_5ce=_5ca.y+(_5cb.charAt(0)=="T"?0:_5c9);
pos=dojo.html.placeOnScreen(node,_5cd,_5ce,_5c0,true,_5cf,true);
if(pos.dist==0){
best=pos;
break;
}else{
if(_5c5>pos.dist){
_5c5=pos.dist;
best=pos;
}
}
}
if(!_5c3){
node.style.left=best.left+"px";
node.style.top=best.top+"px";
}
return best;
};
dojo.html.scrollIntoView=function(node){
if(!node){
return;
}
if(dojo.render.html.ie){
if(dojo.html.getBorderBox(node.parentNode).height<=node.parentNode.scrollHeight){
node.scrollIntoView(false);
}
}else{
if(dojo.render.html.mozilla){
node.scrollIntoView(false);
}else{
var _5d1=node.parentNode;
var _5d2=_5d1.scrollTop+dojo.html.getBorderBox(_5d1).height;
var _5d3=node.offsetTop+dojo.html.getMarginBox(node).height;
if(_5d2<_5d3){
_5d1.scrollTop+=(_5d3-_5d2);
}else{
if(_5d1.scrollTop>node.offsetTop){
_5d1.scrollTop-=(_5d1.scrollTop-node.offsetTop);
}
}
}
}
};
dojo.provide("dojo.gfx.color");
dojo.gfx.color.Color=function(r,g,b,a){
if(dojo.lang.isArray(r)){
this.r=r[0];
this.g=r[1];
this.b=r[2];
this.a=r[3]||1;
}else{
if(dojo.lang.isString(r)){
var rgb=dojo.gfx.color.extractRGB(r);
this.r=rgb[0];
this.g=rgb[1];
this.b=rgb[2];
this.a=g||1;
}else{
if(r instanceof dojo.gfx.color.Color){
this.r=r.r;
this.b=r.b;
this.g=r.g;
this.a=r.a;
}else{
this.r=r;
this.g=g;
this.b=b;
this.a=a;
}
}
}
};
dojo.gfx.color.Color.fromArray=function(arr){
return new dojo.gfx.color.Color(arr[0],arr[1],arr[2],arr[3]);
};
dojo.extend(dojo.gfx.color.Color,{toRgb:function(_5da){
if(_5da){
return this.toRgba();
}else{
return [this.r,this.g,this.b];
}
},toRgba:function(){
return [this.r,this.g,this.b,this.a];
},toHex:function(){
return dojo.gfx.color.rgb2hex(this.toRgb());
},toCss:function(){
return "rgb("+this.toRgb().join()+")";
},toString:function(){
return this.toHex();
},blend:function(_5db,_5dc){
var rgb=null;
if(dojo.lang.isArray(_5db)){
rgb=_5db;
}else{
if(_5db instanceof dojo.gfx.color.Color){
rgb=_5db.toRgb();
}else{
rgb=new dojo.gfx.color.Color(_5db).toRgb();
}
}
return dojo.gfx.color.blend(this.toRgb(),rgb,_5dc);
}});
dojo.gfx.color.named={white:[255,255,255],black:[0,0,0],red:[255,0,0],green:[0,255,0],lime:[0,255,0],blue:[0,0,255],navy:[0,0,128],gray:[128,128,128],silver:[192,192,192]};
dojo.gfx.color.blend=function(a,b,_5e0){
if(typeof a=="string"){
return dojo.gfx.color.blendHex(a,b,_5e0);
}
if(!_5e0){
_5e0=0;
}
_5e0=Math.min(Math.max(-1,_5e0),1);
_5e0=((_5e0+1)/2);
var c=[];
for(var x=0;x<3;x++){
c[x]=parseInt(b[x]+((a[x]-b[x])*_5e0));
}
return c;
};
dojo.gfx.color.blendHex=function(a,b,_5e5){
return dojo.gfx.color.rgb2hex(dojo.gfx.color.blend(dojo.gfx.color.hex2rgb(a),dojo.gfx.color.hex2rgb(b),_5e5));
};
dojo.gfx.color.extractRGB=function(_5e6){
var hex="0123456789abcdef";
_5e6=_5e6.toLowerCase();
if(_5e6.indexOf("rgb")==0){
var _5e8=_5e6.match(/rgba*\((\d+), *(\d+), *(\d+)/i);
var ret=_5e8.splice(1,3);
return ret;
}else{
var _5ea=dojo.gfx.color.hex2rgb(_5e6);
if(_5ea){
return _5ea;
}else{
return dojo.gfx.color.named[_5e6]||[255,255,255];
}
}
};
dojo.gfx.color.hex2rgb=function(hex){
var _5ec="0123456789ABCDEF";
var rgb=new Array(3);
if(hex.indexOf("#")==0){
hex=hex.substring(1);
}
hex=hex.toUpperCase();
if(hex.replace(new RegExp("["+_5ec+"]","g"),"")!=""){
return null;
}
if(hex.length==3){
rgb[0]=hex.charAt(0)+hex.charAt(0);
rgb[1]=hex.charAt(1)+hex.charAt(1);
rgb[2]=hex.charAt(2)+hex.charAt(2);
}else{
rgb[0]=hex.substring(0,2);
rgb[1]=hex.substring(2,4);
rgb[2]=hex.substring(4);
}
for(var i=0;i<rgb.length;i++){
rgb[i]=_5ec.indexOf(rgb[i].charAt(0))*16+_5ec.indexOf(rgb[i].charAt(1));
}
return rgb;
};
dojo.gfx.color.rgb2hex=function(r,g,b){
if(dojo.lang.isArray(r)){
g=r[1]||0;
b=r[2]||0;
r=r[0]||0;
}
var ret=dojo.lang.map([r,g,b],function(x){
x=new Number(x);
var s=x.toString(16);
while(s.length<2){
s="0"+s;
}
return s;
});
ret.unshift("#");
return ret.join("");
};
dojo.provide("dojo.lfx.Animation");
dojo.lfx.Line=function(_5f5,end){
this.start=_5f5;
this.end=end;
if(dojo.lang.isArray(_5f5)){
var diff=[];
dojo.lang.forEach(this.start,function(s,i){
diff[i]=this.end[i]-s;
},this);
this.getValue=function(n){
var res=[];
dojo.lang.forEach(this.start,function(s,i){
res[i]=(diff[i]*n)+s;
},this);
return res;
};
}else{
var diff=end-_5f5;
this.getValue=function(n){
return (diff*n)+this.start;
};
}
};
if((dojo.render.html.khtml)&&(!dojo.render.html.safari)){
dojo.lfx.easeDefault=function(n){
return (parseFloat("0.5")+((Math.sin((n+parseFloat("1.5"))*Math.PI))/2));
};
}else{
dojo.lfx.easeDefault=function(n){
return (0.5+((Math.sin((n+1.5)*Math.PI))/2));
};
}
dojo.lfx.easeIn=function(n){
return Math.pow(n,3);
};
dojo.lfx.easeOut=function(n){
return (1-Math.pow(1-n,3));
};
dojo.lfx.easeInOut=function(n){
return ((3*Math.pow(n,2))-(2*Math.pow(n,3)));
};
dojo.lfx.IAnimation=function(){
};
dojo.lang.extend(dojo.lfx.IAnimation,{curve:null,duration:1000,easing:null,repeatCount:0,rate:10,handler:null,beforeBegin:null,onBegin:null,onAnimate:null,onEnd:null,onPlay:null,onPause:null,onStop:null,play:null,pause:null,stop:null,connect:function(evt,_605,_606){
if(!_606){
_606=_605;
_605=this;
}
_606=dojo.lang.hitch(_605,_606);
var _607=this[evt]||function(){
};
this[evt]=function(){
var ret=_607.apply(this,arguments);
_606.apply(this,arguments);
return ret;
};
return this;
},fire:function(evt,args){
if(this[evt]){
this[evt].apply(this,(args||[]));
}
return this;
},repeat:function(_60b){
this.repeatCount=_60b;
return this;
},_active:false,_paused:false});
dojo.lfx.Animation=function(_60c,_60d,_60e,_60f,_610,rate){
dojo.lfx.IAnimation.call(this);
if(dojo.lang.isNumber(_60c)||(!_60c&&_60d.getValue)){
rate=_610;
_610=_60f;
_60f=_60e;
_60e=_60d;
_60d=_60c;
_60c=null;
}else{
if(_60c.getValue||dojo.lang.isArray(_60c)){
rate=_60f;
_610=_60e;
_60f=_60d;
_60e=_60c;
_60d=null;
_60c=null;
}
}
if(dojo.lang.isArray(_60e)){
this.curve=new dojo.lfx.Line(_60e[0],_60e[1]);
}else{
this.curve=_60e;
}
if(_60d!=null&&_60d>0){
this.duration=_60d;
}
if(_610){
this.repeatCount=_610;
}
if(rate){
this.rate=rate;
}
if(_60c){
dojo.lang.forEach(["handler","beforeBegin","onBegin","onEnd","onPlay","onStop","onAnimate"],function(item){
if(_60c[item]){
this.connect(item,_60c[item]);
}
},this);
}
if(_60f&&dojo.lang.isFunction(_60f)){
this.easing=_60f;
}
};
dojo.inherits(dojo.lfx.Animation,dojo.lfx.IAnimation);
dojo.lang.extend(dojo.lfx.Animation,{_startTime:null,_endTime:null,_timer:null,_percent:0,_startRepeatCount:0,play:function(_613,_614){
if(_614){
clearTimeout(this._timer);
this._active=false;
this._paused=false;
this._percent=0;
}else{
if(this._active&&!this._paused){
return this;
}
}
this.fire("handler",["beforeBegin"]);
this.fire("beforeBegin");
if(_613>0){
setTimeout(dojo.lang.hitch(this,function(){
this.play(null,_614);
}),_613);
return this;
}
this._startTime=new Date().valueOf();
if(this._paused){
this._startTime-=(this.duration*this._percent/100);
}
this._endTime=this._startTime+this.duration;
this._active=true;
this._paused=false;
var step=this._percent/100;
var _616=this.curve.getValue(step);
if(this._percent==0){
if(!this._startRepeatCount){
this._startRepeatCount=this.repeatCount;
}
this.fire("handler",["begin",_616]);
this.fire("onBegin",[_616]);
}
this.fire("handler",["play",_616]);
this.fire("onPlay",[_616]);
this._cycle();
return this;
},pause:function(){
clearTimeout(this._timer);
if(!this._active){
return this;
}
this._paused=true;
var _617=this.curve.getValue(this._percent/100);
this.fire("handler",["pause",_617]);
this.fire("onPause",[_617]);
return this;
},gotoPercent:function(pct,_619){
clearTimeout(this._timer);
this._active=true;
this._paused=true;
this._percent=pct;
if(_619){
this.play();
}
return this;
},stop:function(_61a){
clearTimeout(this._timer);
var step=this._percent/100;
if(_61a){
step=1;
}
var _61c=this.curve.getValue(step);
this.fire("handler",["stop",_61c]);
this.fire("onStop",[_61c]);
this._active=false;
this._paused=false;
return this;
},status:function(){
if(this._active){
return this._paused?"paused":"playing";
}else{
return "stopped";
}
return this;
},_cycle:function(){
clearTimeout(this._timer);
if(this._active){
var curr=new Date().valueOf();
var step=(curr-this._startTime)/(this._endTime-this._startTime);
if(step>=1){
step=1;
this._percent=100;
}else{
this._percent=step*100;
}
if((this.easing)&&(dojo.lang.isFunction(this.easing))){
step=this.easing(step);
}
var _61f=this.curve.getValue(step);
this.fire("handler",["animate",_61f]);
this.fire("onAnimate",[_61f]);
if(step<1){
this._timer=setTimeout(dojo.lang.hitch(this,"_cycle"),this.rate);
}else{
this._active=false;
this.fire("handler",["end"]);
this.fire("onEnd");
if(this.repeatCount>0){
this.repeatCount--;
this.play(null,true);
}else{
if(this.repeatCount==-1){
this.play(null,true);
}else{
if(this._startRepeatCount){
this.repeatCount=this._startRepeatCount;
this._startRepeatCount=0;
}
}
}
}
}
return this;
}});
dojo.lfx.Combine=function(_620){
dojo.lfx.IAnimation.call(this);
this._anims=[];
this._animsEnded=0;
var _621=arguments;
if(_621.length==1&&(dojo.lang.isArray(_621[0])||dojo.lang.isArrayLike(_621[0]))){
_621=_621[0];
}
dojo.lang.forEach(_621,function(anim){
this._anims.push(anim);
anim.connect("onEnd",dojo.lang.hitch(this,"_onAnimsEnded"));
},this);
};
dojo.inherits(dojo.lfx.Combine,dojo.lfx.IAnimation);
dojo.lang.extend(dojo.lfx.Combine,{_animsEnded:0,play:function(_623,_624){
if(!this._anims.length){
return this;
}
this.fire("beforeBegin");
if(_623>0){
setTimeout(dojo.lang.hitch(this,function(){
this.play(null,_624);
}),_623);
return this;
}
if(_624||this._anims[0].percent==0){
this.fire("onBegin");
}
this.fire("onPlay");
this._animsCall("play",null,_624);
return this;
},pause:function(){
this.fire("onPause");
this._animsCall("pause");
return this;
},stop:function(_625){
this.fire("onStop");
this._animsCall("stop",_625);
return this;
},_onAnimsEnded:function(){
this._animsEnded++;
if(this._animsEnded>=this._anims.length){
this.fire("onEnd");
}
return this;
},_animsCall:function(_626){
var args=[];
if(arguments.length>1){
for(var i=1;i<arguments.length;i++){
args.push(arguments[i]);
}
}
var _629=this;
dojo.lang.forEach(this._anims,function(anim){
anim[_626](args);
},_629);
return this;
}});
dojo.lfx.Chain=function(_62b){
dojo.lfx.IAnimation.call(this);
this._anims=[];
this._currAnim=-1;
var _62c=arguments;
if(_62c.length==1&&(dojo.lang.isArray(_62c[0])||dojo.lang.isArrayLike(_62c[0]))){
_62c=_62c[0];
}
var _62d=this;
dojo.lang.forEach(_62c,function(anim,i,_630){
this._anims.push(anim);
if(i<_630.length-1){
anim.connect("onEnd",dojo.lang.hitch(this,"_playNext"));
}else{
anim.connect("onEnd",dojo.lang.hitch(this,function(){
this.fire("onEnd");
}));
}
},this);
};
dojo.inherits(dojo.lfx.Chain,dojo.lfx.IAnimation);
dojo.lang.extend(dojo.lfx.Chain,{_currAnim:-1,play:function(_631,_632){
if(!this._anims.length){
return this;
}
if(_632||!this._anims[this._currAnim]){
this._currAnim=0;
}
var _633=this._anims[this._currAnim];
this.fire("beforeBegin");
if(_631>0){
setTimeout(dojo.lang.hitch(this,function(){
this.play(null,_632);
}),_631);
return this;
}
if(_633){
if(this._currAnim==0){
this.fire("handler",["begin",this._currAnim]);
this.fire("onBegin",[this._currAnim]);
}
this.fire("onPlay",[this._currAnim]);
_633.play(null,_632);
}
return this;
},pause:function(){
if(this._anims[this._currAnim]){
this._anims[this._currAnim].pause();
this.fire("onPause",[this._currAnim]);
}
return this;
},playPause:function(){
if(this._anims.length==0){
return this;
}
if(this._currAnim==-1){
this._currAnim=0;
}
var _634=this._anims[this._currAnim];
if(_634){
if(!_634._active||_634._paused){
this.play();
}else{
this.pause();
}
}
return this;
},stop:function(){
var _635=this._anims[this._currAnim];
if(_635){
_635.stop();
this.fire("onStop",[this._currAnim]);
}
return _635;
},_playNext:function(){
if(this._currAnim==-1||this._anims.length==0){
return this;
}
this._currAnim++;
if(this._anims[this._currAnim]){
this._anims[this._currAnim].play(null,true);
}
return this;
}});
dojo.lfx.combine=function(_636){
var _637=arguments;
if(dojo.lang.isArray(arguments[0])){
_637=arguments[0];
}
if(_637.length==1){
return _637[0];
}
return new dojo.lfx.Combine(_637);
};
dojo.lfx.chain=function(_638){
var _639=arguments;
if(dojo.lang.isArray(arguments[0])){
_639=arguments[0];
}
if(_639.length==1){
return _639[0];
}
return new dojo.lfx.Chain(_639);
};
dojo.provide("dojo.html.color");
dojo.html.getBackgroundColor=function(node){
node=dojo.byId(node);
var _63b;
do{
_63b=dojo.html.getStyle(node,"background-color");
if(_63b.toLowerCase()=="rgba(0, 0, 0, 0)"){
_63b="transparent";
}
if(node==document.getElementsByTagName("body")[0]){
node=null;
break;
}
node=node.parentNode;
}while(node&&dojo.lang.inArray(["transparent",""],_63b));
if(_63b=="transparent"){
_63b=[255,255,255,0];
}else{
_63b=dojo.gfx.color.extractRGB(_63b);
}
return _63b;
};
dojo.provide("dojo.lfx.html");
dojo.lfx.html._byId=function(_63c){
if(!_63c){
return [];
}
if(dojo.lang.isArrayLike(_63c)){
if(!_63c.alreadyChecked){
var n=[];
dojo.lang.forEach(_63c,function(node){
n.push(dojo.byId(node));
});
n.alreadyChecked=true;
return n;
}else{
return _63c;
}
}else{
var n=[];
n.push(dojo.byId(_63c));
n.alreadyChecked=true;
return n;
}
};
dojo.lfx.html.propertyAnimation=function(_63f,_640,_641,_642,_643){
_63f=dojo.lfx.html._byId(_63f);
var _644={"propertyMap":_640,"nodes":_63f,"duration":_641,"easing":_642||dojo.lfx.easeDefault};
var _645=function(args){
if(args.nodes.length==1){
var pm=args.propertyMap;
if(!dojo.lang.isArray(args.propertyMap)){
var parr=[];
for(var _649 in pm){
pm[_649].property=_649;
parr.push(pm[_649]);
}
pm=args.propertyMap=parr;
}
dojo.lang.forEach(pm,function(prop){
if(dj_undef("start",prop)){
if(prop.property!="opacity"){
prop.start=parseInt(dojo.html.getComputedStyle(args.nodes[0],prop.property));
}else{
prop.start=dojo.html.getOpacity(args.nodes[0]);
}
}
});
}
};
var _64b=function(_64c){
var _64d=[];
dojo.lang.forEach(_64c,function(c){
_64d.push(Math.round(c));
});
return _64d;
};
var _64f=function(n,_651){
n=dojo.byId(n);
if(!n||!n.style){
return;
}
for(var s in _651){
try{
if(s=="opacity"){
dojo.html.setOpacity(n,_651[s]);
}else{
n.style[s]=_651[s];
}
}
catch(e){
dojo.debug(e);
}
}
};
var _653=function(_654){
this._properties=_654;
this.diffs=new Array(_654.length);
dojo.lang.forEach(_654,function(prop,i){
if(dojo.lang.isFunction(prop.start)){
prop.start=prop.start(prop,i);
}
if(dojo.lang.isFunction(prop.end)){
prop.end=prop.end(prop,i);
}
if(dojo.lang.isArray(prop.start)){
this.diffs[i]=null;
}else{
if(prop.start instanceof dojo.gfx.color.Color){
prop.startRgb=prop.start.toRgb();
prop.endRgb=prop.end.toRgb();
}else{
this.diffs[i]=prop.end-prop.start;
}
}
},this);
this.getValue=function(n){
var ret={};
dojo.lang.forEach(this._properties,function(prop,i){
var _65b=null;
if(dojo.lang.isArray(prop.start)){
}else{
if(prop.start instanceof dojo.gfx.color.Color){
_65b=(prop.units||"rgb")+"(";
for(var j=0;j<prop.startRgb.length;j++){
_65b+=Math.round(((prop.endRgb[j]-prop.startRgb[j])*n)+prop.startRgb[j])+(j<prop.startRgb.length-1?",":"");
}
_65b+=")";
}else{
_65b=((this.diffs[i])*n)+prop.start+(prop.property!="opacity"?prop.units||"px":"");
}
}
ret[dojo.html.toCamelCase(prop.property)]=_65b;
},this);
return ret;
};
};
var anim=new dojo.lfx.Animation({beforeBegin:function(){
_645(_644);
anim.curve=new _653(_644.propertyMap);
},onAnimate:function(_65e){
dojo.lang.forEach(_644.nodes,function(node){
_64f(node,_65e);
});
}},_644.duration,null,_644.easing);
if(_643){
for(var x in _643){
if(dojo.lang.isFunction(_643[x])){
anim.connect(x,anim,_643[x]);
}
}
}
return anim;
};
dojo.lfx.html._makeFadeable=function(_661){
var _662=function(node){
if(dojo.render.html.ie){
if((node.style.zoom.length==0)&&(dojo.html.getStyle(node,"zoom")=="normal")){
node.style.zoom="1";
}
if((node.style.width.length==0)&&(dojo.html.getStyle(node,"width")=="auto")){
node.style.width="auto";
}
}
};
if(dojo.lang.isArrayLike(_661)){
dojo.lang.forEach(_661,_662);
}else{
_662(_661);
}
};
dojo.lfx.html.fade=function(_664,_665,_666,_667,_668){
_664=dojo.lfx.html._byId(_664);
var _669={property:"opacity"};
if(!dj_undef("start",_665)){
_669.start=_665.start;
}else{
_669.start=function(){
return dojo.html.getOpacity(_664[0]);
};
}
if(!dj_undef("end",_665)){
_669.end=_665.end;
}else{
dojo.raise("dojo.lfx.html.fade needs an end value");
}
var anim=dojo.lfx.propertyAnimation(_664,[_669],_666,_667);
anim.connect("beforeBegin",function(){
dojo.lfx.html._makeFadeable(_664);
});
if(_668){
anim.connect("onEnd",function(){
_668(_664,anim);
});
}
return anim;
};
dojo.lfx.html.fadeIn=function(_66b,_66c,_66d,_66e){
return dojo.lfx.html.fade(_66b,{end:1},_66c,_66d,_66e);
};
dojo.lfx.html.fadeOut=function(_66f,_670,_671,_672){
return dojo.lfx.html.fade(_66f,{end:0},_670,_671,_672);
};
dojo.lfx.html.fadeShow=function(_673,_674,_675,_676){
_673=dojo.lfx.html._byId(_673);
dojo.lang.forEach(_673,function(node){
dojo.html.setOpacity(node,0);
});
var anim=dojo.lfx.html.fadeIn(_673,_674,_675,_676);
anim.connect("beforeBegin",function(){
if(dojo.lang.isArrayLike(_673)){
dojo.lang.forEach(_673,dojo.html.show);
}else{
dojo.html.show(_673);
}
});
return anim;
};
dojo.lfx.html.fadeHide=function(_679,_67a,_67b,_67c){
var anim=dojo.lfx.html.fadeOut(_679,_67a,_67b,function(){
if(dojo.lang.isArrayLike(_679)){
dojo.lang.forEach(_679,dojo.html.hide);
}else{
dojo.html.hide(_679);
}
if(_67c){
_67c(_679,anim);
}
});
return anim;
};
dojo.lfx.html.wipeIn=function(_67e,_67f,_680,_681){
_67e=dojo.lfx.html._byId(_67e);
var _682=[];
dojo.lang.forEach(_67e,function(node){
var _684={};
var _685,_686,_687;
with(node.style){
_685=top;
_686=left;
_687=position;
top="-9999px";
left="-9999px";
position="absolute";
display="";
}
var _688=dojo.html.getBorderBox(node).height;
with(node.style){
top=_685;
left=_686;
position=_687;
display="none";
}
var anim=dojo.lfx.propertyAnimation(node,{"height":{start:1,end:function(){
return _688;
}}},_67f,_680);
anim.connect("beforeBegin",function(){
_684.overflow=node.style.overflow;
_684.height=node.style.height;
with(node.style){
overflow="hidden";
_688="1px";
}
dojo.html.show(node);
});
anim.connect("onEnd",function(){
with(node.style){
overflow=_684.overflow;
_688=_684.height;
}
if(_681){
_681(node,anim);
}
});
_682.push(anim);
});
return dojo.lfx.combine(_682);
};
dojo.lfx.html.wipeOut=function(_68a,_68b,_68c,_68d){
_68a=dojo.lfx.html._byId(_68a);
var _68e=[];
dojo.lang.forEach(_68a,function(node){
var _690={};
var anim=dojo.lfx.propertyAnimation(node,{"height":{start:function(){
return dojo.html.getContentBox(node).height;
},end:1}},_68b,_68c,{"beforeBegin":function(){
_690.overflow=node.style.overflow;
_690.height=node.style.height;
with(node.style){
overflow="hidden";
}
dojo.html.show(node);
},"onEnd":function(){
dojo.html.hide(node);
with(node.style){
overflow=_690.overflow;
height=_690.height;
}
if(_68d){
_68d(node,anim);
}
}});
_68e.push(anim);
});
return dojo.lfx.combine(_68e);
};
dojo.lfx.html.slideTo=function(_692,_693,_694,_695,_696){
_692=dojo.lfx.html._byId(_692);
var _697=[];
var _698=dojo.html.getComputedStyle;
if(dojo.lang.isArray(_693)){
dojo.deprecated("dojo.lfx.html.slideTo(node, array)","use dojo.lfx.html.slideTo(node, {top: value, left: value});","0.5");
_693={top:_693[0],left:_693[1]};
}
dojo.lang.forEach(_692,function(node){
var top=null;
var left=null;
var init=(function(){
var _69d=node;
return function(){
var pos=_698(_69d,"position");
top=(pos=="absolute"?node.offsetTop:parseInt(_698(node,"top"))||0);
left=(pos=="absolute"?node.offsetLeft:parseInt(_698(node,"left"))||0);
if(!dojo.lang.inArray(["absolute","relative"],pos)){
var ret=dojo.html.abs(_69d,true);
dojo.html.setStyleAttributes(_69d,"position:absolute;top:"+ret.y+"px;left:"+ret.x+"px;");
top=ret.y;
left=ret.x;
}
};
})();
init();
var anim=dojo.lfx.propertyAnimation(node,{"top":{start:top,end:(_693.top||0)},"left":{start:left,end:(_693.left||0)}},_694,_695,{"beforeBegin":init});
if(_696){
anim.connect("onEnd",function(){
_696(_692,anim);
});
}
_697.push(anim);
});
return dojo.lfx.combine(_697);
};
dojo.lfx.html.slideBy=function(_6a1,_6a2,_6a3,_6a4,_6a5){
_6a1=dojo.lfx.html._byId(_6a1);
var _6a6=[];
var _6a7=dojo.html.getComputedStyle;
if(dojo.lang.isArray(_6a2)){
dojo.deprecated("dojo.lfx.html.slideBy(node, array)","use dojo.lfx.html.slideBy(node, {top: value, left: value});","0.5");
_6a2={top:_6a2[0],left:_6a2[1]};
}
dojo.lang.forEach(_6a1,function(node){
var top=null;
var left=null;
var init=(function(){
var _6ac=node;
return function(){
var pos=_6a7(_6ac,"position");
top=(pos=="absolute"?node.offsetTop:parseInt(_6a7(node,"top"))||0);
left=(pos=="absolute"?node.offsetLeft:parseInt(_6a7(node,"left"))||0);
if(!dojo.lang.inArray(["absolute","relative"],pos)){
var ret=dojo.html.abs(_6ac,true);
dojo.html.setStyleAttributes(_6ac,"position:absolute;top:"+ret.y+"px;left:"+ret.x+"px;");
top=ret.y;
left=ret.x;
}
};
})();
init();
var anim=dojo.lfx.propertyAnimation(node,{"top":{start:top,end:top+(_6a2.top||0)},"left":{start:left,end:left+(_6a2.left||0)}},_6a3,_6a4).connect("beforeBegin",init);
if(_6a5){
anim.connect("onEnd",function(){
_6a5(_6a1,anim);
});
}
_6a6.push(anim);
});
return dojo.lfx.combine(_6a6);
};
dojo.lfx.html.explode=function(_6b0,_6b1,_6b2,_6b3,_6b4){
var h=dojo.html;
_6b0=dojo.byId(_6b0);
_6b1=dojo.byId(_6b1);
var _6b6=h.toCoordinateObject(_6b0,true);
var _6b7=document.createElement("div");
h.copyStyle(_6b7,_6b1);
if(_6b1.explodeClassName){
_6b7.className=_6b1.explodeClassName;
}
with(_6b7.style){
position="absolute";
display="none";
var _6b8=h.getStyle(_6b0,"background-color");
backgroundColor=_6b8?_6b8.toLowerCase():"transparent";
backgroundColor=(backgroundColor=="transparent")?"rgb(221, 221, 221)":backgroundColor;
}
dojo.body().appendChild(_6b7);
with(_6b1.style){
visibility="hidden";
display="block";
}
var _6b9=h.toCoordinateObject(_6b1,true);
with(_6b1.style){
display="none";
visibility="visible";
}
var _6ba={opacity:{start:0.5,end:1}};
dojo.lang.forEach(["height","width","top","left"],function(type){
_6ba[type]={start:_6b6[type],end:_6b9[type]};
});
var anim=new dojo.lfx.propertyAnimation(_6b7,_6ba,_6b2,_6b3,{"beforeBegin":function(){
h.setDisplay(_6b7,"block");
},"onEnd":function(){
h.setDisplay(_6b1,"block");
_6b7.parentNode.removeChild(_6b7);
}});
if(_6b4){
anim.connect("onEnd",function(){
_6b4(_6b1,anim);
});
}
return anim;
};
dojo.lfx.html.implode=function(_6bd,end,_6bf,_6c0,_6c1){
var h=dojo.html;
_6bd=dojo.byId(_6bd);
end=dojo.byId(end);
var _6c3=dojo.html.toCoordinateObject(_6bd,true);
var _6c4=dojo.html.toCoordinateObject(end,true);
var _6c5=document.createElement("div");
dojo.html.copyStyle(_6c5,_6bd);
if(_6bd.explodeClassName){
_6c5.className=_6bd.explodeClassName;
}
dojo.html.setOpacity(_6c5,0.3);
with(_6c5.style){
position="absolute";
display="none";
backgroundColor=h.getStyle(_6bd,"background-color").toLowerCase();
}
dojo.body().appendChild(_6c5);
var _6c6={opacity:{start:1,end:0.5}};
dojo.lang.forEach(["height","width","top","left"],function(type){
_6c6[type]={start:_6c3[type],end:_6c4[type]};
});
var anim=new dojo.lfx.propertyAnimation(_6c5,_6c6,_6bf,_6c0,{"beforeBegin":function(){
dojo.html.hide(_6bd);
dojo.html.show(_6c5);
},"onEnd":function(){
_6c5.parentNode.removeChild(_6c5);
}});
if(_6c1){
anim.connect("onEnd",function(){
_6c1(_6bd,anim);
});
}
return anim;
};
dojo.lfx.html.highlight=function(_6c9,_6ca,_6cb,_6cc,_6cd){
_6c9=dojo.lfx.html._byId(_6c9);
var _6ce=[];
dojo.lang.forEach(_6c9,function(node){
var _6d0=dojo.html.getBackgroundColor(node);
var bg=dojo.html.getStyle(node,"background-color").toLowerCase();
var _6d2=dojo.html.getStyle(node,"background-image");
var _6d3=(bg=="transparent"||bg=="rgba(0, 0, 0, 0)");
while(_6d0.length>3){
_6d0.pop();
}
var rgb=new dojo.gfx.color.Color(_6ca);
var _6d5=new dojo.gfx.color.Color(_6d0);
var anim=dojo.lfx.propertyAnimation(node,{"background-color":{start:rgb,end:_6d5}},_6cb,_6cc,{"beforeBegin":function(){
if(_6d2){
node.style.backgroundImage="none";
}
node.style.backgroundColor="rgb("+rgb.toRgb().join(",")+")";
},"onEnd":function(){
if(_6d2){
node.style.backgroundImage=_6d2;
}
if(_6d3){
node.style.backgroundColor="transparent";
}
if(_6cd){
_6cd(node,anim);
}
}});
_6ce.push(anim);
});
return dojo.lfx.combine(_6ce);
};
dojo.lfx.html.unhighlight=function(_6d7,_6d8,_6d9,_6da,_6db){
_6d7=dojo.lfx.html._byId(_6d7);
var _6dc=[];
dojo.lang.forEach(_6d7,function(node){
var _6de=new dojo.gfx.color.Color(dojo.html.getBackgroundColor(node));
var rgb=new dojo.gfx.color.Color(_6d8);
var _6e0=dojo.html.getStyle(node,"background-image");
var anim=dojo.lfx.propertyAnimation(node,{"background-color":{start:_6de,end:rgb}},_6d9,_6da,{"beforeBegin":function(){
if(_6e0){
node.style.backgroundImage="none";
}
node.style.backgroundColor="rgb("+_6de.toRgb().join(",")+")";
},"onEnd":function(){
if(_6db){
_6db(node,anim);
}
}});
_6dc.push(anim);
});
return dojo.lfx.combine(_6dc);
};
dojo.lang.mixin(dojo.lfx,dojo.lfx.html);
dojo.kwCompoundRequire({browser:["dojo.lfx.html"],dashboard:["dojo.lfx.html"]});
dojo.provide("dojo.lfx.*");
dojo.provide("dojo.lfx.toggle");
dojo.lfx.toggle.plain={show:function(node,_6e3,_6e4,_6e5){
dojo.html.show(node);
if(dojo.lang.isFunction(_6e5)){
_6e5();
}
},hide:function(node,_6e7,_6e8,_6e9){
dojo.html.hide(node);
if(dojo.lang.isFunction(_6e9)){
_6e9();
}
}};
dojo.lfx.toggle.fade={show:function(node,_6eb,_6ec,_6ed){
dojo.lfx.fadeShow(node,_6eb,_6ec,_6ed).play();
},hide:function(node,_6ef,_6f0,_6f1){
dojo.lfx.fadeHide(node,_6ef,_6f0,_6f1).play();
}};
dojo.lfx.toggle.wipe={show:function(node,_6f3,_6f4,_6f5){
dojo.lfx.wipeIn(node,_6f3,_6f4,_6f5).play();
},hide:function(node,_6f7,_6f8,_6f9){
dojo.lfx.wipeOut(node,_6f7,_6f8,_6f9).play();
}};
dojo.lfx.toggle.explode={show:function(node,_6fb,_6fc,_6fd,_6fe){
dojo.lfx.explode(_6fe||{x:0,y:0,width:0,height:0},node,_6fb,_6fc,_6fd).play();
},hide:function(node,_700,_701,_702,_703){
dojo.lfx.implode(node,_703||{x:0,y:0,width:0,height:0},_700,_701,_702).play();
}};
dojo.provide("dojo.widget.HtmlWidget");
dojo.declare("dojo.widget.HtmlWidget",dojo.widget.DomWidget,{templateCssPath:null,templatePath:null,lang:"",toggle:"plain",toggleDuration:150,initialize:function(args,frag){
},postMixInProperties:function(args,frag){
if(this.lang===""){
this.lang=null;
}
this.toggleObj=dojo.lfx.toggle[this.toggle.toLowerCase()]||dojo.lfx.toggle.plain;
},createNodesFromText:function(txt,wrap){
return dojo.html.createNodesFromText(txt,wrap);
},destroyRendering:function(_70a){
try{
if(this.bgIframe){
this.bgIframe.remove();
delete this.bgIframe;
}
if(!_70a&&this.domNode){
dojo.event.browser.clean(this.domNode);
}
dojo.widget.HtmlWidget.superclass.destroyRendering.call(this);
}
catch(e){
}
},isShowing:function(){
return dojo.html.isShowing(this.domNode);
},toggleShowing:function(){
if(this.isShowing()){
this.hide();
}else{
this.show();
}
},show:function(){
if(this.isShowing()){
return;
}
this.animationInProgress=true;
this.toggleObj.show(this.domNode,this.toggleDuration,null,dojo.lang.hitch(this,this.onShow),this.explodeSrc);
},onShow:function(){
this.animationInProgress=false;
this.checkSize();
},hide:function(){
if(!this.isShowing()){
return;
}
this.animationInProgress=true;
this.toggleObj.hide(this.domNode,this.toggleDuration,null,dojo.lang.hitch(this,this.onHide),this.explodeSrc);
},onHide:function(){
this.animationInProgress=false;
},_isResized:function(w,h){
if(!this.isShowing()){
return false;
}
var wh=dojo.html.getMarginBox(this.domNode);
var _70e=w||wh.width;
var _70f=h||wh.height;
if(this.width==_70e&&this.height==_70f){
return false;
}
this.width=_70e;
this.height=_70f;
return true;
},checkSize:function(){
if(!this._isResized()){
return;
}
this.onResized();
},resizeTo:function(w,h){
dojo.html.setMarginBox(this.domNode,{width:w,height:h});
if(this.isShowing()){
this.onResized();
}
},resizeSoon:function(){
if(this.isShowing()){
dojo.lang.setTimeout(this,this.onResized,0);
}
},onResized:function(){
dojo.lang.forEach(this.children,function(_712){
if(_712.checkSize){
_712.checkSize();
}
});
}});
dojo.kwCompoundRequire({common:["dojo.xml.Parse","dojo.widget.Widget","dojo.widget.Parse","dojo.widget.Manager"],browser:["dojo.widget.DomWidget","dojo.widget.HtmlWidget"],dashboard:["dojo.widget.DomWidget","dojo.widget.HtmlWidget"],svg:["dojo.widget.SvgWidget"],rhino:["dojo.widget.SwtWidget"]});
dojo.provide("dojo.widget.*");
dojo.provide("dojo.string.common");
dojo.string.trim=function(str,wh){
if(!str.replace){
return str;
}
if(!str.length){
return str;
}
var re=(wh>0)?(/^\s+/):(wh<0)?(/\s+$/):(/^\s+|\s+$/g);
return str.replace(re,"");
};
dojo.string.trimStart=function(str){
return dojo.string.trim(str,1);
};
dojo.string.trimEnd=function(str){
return dojo.string.trim(str,-1);
};
dojo.string.repeat=function(str,_719,_71a){
var out="";
for(var i=0;i<_719;i++){
out+=str;
if(_71a&&i<_719-1){
out+=_71a;
}
}
return out;
};
dojo.string.pad=function(str,len,c,dir){
var out=String(str);
if(!c){
c="0";
}
if(!dir){
dir=1;
}
while(out.length<len){
if(dir>0){
out=c+out;
}else{
out+=c;
}
}
return out;
};
dojo.string.padLeft=function(str,len,c){
return dojo.string.pad(str,len,c,1);
};
dojo.string.padRight=function(str,len,c){
return dojo.string.pad(str,len,c,-1);
};
dojo.provide("dojo.string");
dojo.provide("dojo.io.common");
dojo.io.transports=[];
dojo.io.hdlrFuncNames=["load","error","timeout"];
dojo.io.Request=function(url,_729,_72a,_72b){
if((arguments.length==1)&&(arguments[0].constructor==Object)){
this.fromKwArgs(arguments[0]);
}else{
this.url=url;
if(_729){
this.mimetype=_729;
}
if(_72a){
this.transport=_72a;
}
if(arguments.length>=4){
this.changeUrl=_72b;
}
}
};
dojo.lang.extend(dojo.io.Request,{url:"",mimetype:"text/plain",method:"GET",content:undefined,transport:undefined,changeUrl:undefined,formNode:undefined,sync:false,bindSuccess:false,useCache:false,preventCache:false,load:function(type,data,_72e,_72f){
},error:function(type,_731,_732,_733){
},timeout:function(type,_735,_736,_737){
},handle:function(type,data,_73a,_73b){
},timeoutSeconds:0,abort:function(){
},fromKwArgs:function(_73c){
if(_73c["url"]){
_73c.url=_73c.url.toString();
}
if(_73c["formNode"]){
_73c.formNode=dojo.byId(_73c.formNode);
}
if(!_73c["method"]&&_73c["formNode"]&&_73c["formNode"].method){
_73c.method=_73c["formNode"].method;
}
if(!_73c["handle"]&&_73c["handler"]){
_73c.handle=_73c.handler;
}
if(!_73c["load"]&&_73c["loaded"]){
_73c.load=_73c.loaded;
}
if(!_73c["changeUrl"]&&_73c["changeURL"]){
_73c.changeUrl=_73c.changeURL;
}
_73c.encoding=dojo.lang.firstValued(_73c["encoding"],djConfig["bindEncoding"],"");
_73c.sendTransport=dojo.lang.firstValued(_73c["sendTransport"],djConfig["ioSendTransport"],false);
var _73d=dojo.lang.isFunction;
for(var x=0;x<dojo.io.hdlrFuncNames.length;x++){
var fn=dojo.io.hdlrFuncNames[x];
if(_73c[fn]&&_73d(_73c[fn])){
continue;
}
if(_73c["handle"]&&_73d(_73c["handle"])){
_73c[fn]=_73c.handle;
}
}
dojo.lang.mixin(this,_73c);
}});
dojo.io.Error=function(msg,type,num){
this.message=msg;
this.type=type||"unknown";
this.number=num||0;
};
dojo.io.transports.addTransport=function(name){
this.push(name);
this[name]=dojo.io[name];
};
dojo.io.bind=function(_744){
if(!(_744 instanceof dojo.io.Request)){
try{
_744=new dojo.io.Request(_744);
}
catch(e){
dojo.debug(e);
}
}
var _745="";
if(_744["transport"]){
_745=_744["transport"];
if(!this[_745]){
dojo.io.sendBindError(_744,"No dojo.io.bind() transport with name '"+_744["transport"]+"'.");
return _744;
}
if(!this[_745].canHandle(_744)){
dojo.io.sendBindError(_744,"dojo.io.bind() transport with name '"+_744["transport"]+"' cannot handle this type of request.");
return _744;
}
}else{
for(var x=0;x<dojo.io.transports.length;x++){
var tmp=dojo.io.transports[x];
if((this[tmp])&&(this[tmp].canHandle(_744))){
_745=tmp;
break;
}
}
if(_745==""){
dojo.io.sendBindError(_744,"None of the loaded transports for dojo.io.bind()"+" can handle the request.");
return _744;
}
}
this[_745].bind(_744);
_744.bindSuccess=true;
return _744;
};
dojo.io.sendBindError=function(_748,_749){
if((typeof _748.error=="function"||typeof _748.handle=="function")&&(typeof setTimeout=="function"||typeof setTimeout=="object")){
var _74a=new dojo.io.Error(_749);
setTimeout(function(){
_748[(typeof _748.error=="function")?"error":"handle"]("error",_74a,null,_748);
},50);
}else{
dojo.raise(_749);
}
};
dojo.io.queueBind=function(_74b){
if(!(_74b instanceof dojo.io.Request)){
try{
_74b=new dojo.io.Request(_74b);
}
catch(e){
dojo.debug(e);
}
}
var _74c=_74b.load;
_74b.load=function(){
dojo.io._queueBindInFlight=false;
var ret=_74c.apply(this,arguments);
dojo.io._dispatchNextQueueBind();
return ret;
};
var _74e=_74b.error;
_74b.error=function(){
dojo.io._queueBindInFlight=false;
var ret=_74e.apply(this,arguments);
dojo.io._dispatchNextQueueBind();
return ret;
};
dojo.io._bindQueue.push(_74b);
dojo.io._dispatchNextQueueBind();
return _74b;
};
dojo.io._dispatchNextQueueBind=function(){
if(!dojo.io._queueBindInFlight){
dojo.io._queueBindInFlight=true;
if(dojo.io._bindQueue.length>0){
dojo.io.bind(dojo.io._bindQueue.shift());
}else{
dojo.io._queueBindInFlight=false;
}
}
};
dojo.io._bindQueue=[];
dojo.io._queueBindInFlight=false;
dojo.io.argsFromMap=function(map,_751,last){
var enc=/utf/i.test(_751||"")?encodeURIComponent:dojo.string.encodeAscii;
var _754=[];
var _755=new Object();
for(var name in map){
var _757=function(elt){
var val=enc(name)+"="+enc(elt);
_754[(last==name)?"push":"unshift"](val);
};
if(!_755[name]){
var _75a=map[name];
if(dojo.lang.isArray(_75a)){
dojo.lang.forEach(_75a,_757);
}else{
_757(_75a);
}
}
}
return _754.join("&");
};
dojo.io.setIFrameSrc=function(_75b,src,_75d){
try{
var r=dojo.render.html;
if(!_75d){
if(r.safari){
_75b.location=src;
}else{
frames[_75b.name].location=src;
}
}else{
var idoc;
if(r.ie){
idoc=_75b.contentWindow.document;
}else{
if(r.safari){
idoc=_75b.document;
}else{
idoc=_75b.contentWindow;
}
}
if(!idoc){
_75b.location=src;
return;
}else{
idoc.location.replace(src);
}
}
}
catch(e){
dojo.debug(e);
dojo.debug("setIFrameSrc: "+e);
}
};
dojo.provide("dojo.string.extras");
dojo.string.substituteParams=function(_760,hash){
var map=(typeof hash=="object")?hash:dojo.lang.toArray(arguments,1);
return _760.replace(/\%\{(\w+)\}/g,function(_763,key){
if(typeof (map[key])!="undefined"&&map[key]!=null){
return map[key];
}
dojo.raise("Substitution not found: "+key);
});
};
dojo.string.capitalize=function(str){
if(!dojo.lang.isString(str)){
return "";
}
if(arguments.length==0){
str=this;
}
var _766=str.split(" ");
for(var i=0;i<_766.length;i++){
_766[i]=_766[i].charAt(0).toUpperCase()+_766[i].substring(1);
}
return _766.join(" ");
};
dojo.string.isBlank=function(str){
if(!dojo.lang.isString(str)){
return true;
}
return (dojo.string.trim(str).length==0);
};
dojo.string.encodeAscii=function(str){
if(!dojo.lang.isString(str)){
return str;
}
var ret="";
var _76b=escape(str);
var _76c,re=/%u([0-9A-F]{4})/i;
while((_76c=_76b.match(re))){
var num=Number("0x"+_76c[1]);
var _76f=escape("&#"+num+";");
ret+=_76b.substring(0,_76c.index)+_76f;
_76b=_76b.substring(_76c.index+_76c[0].length);
}
ret+=_76b.replace(/\+/g,"%2B");
return ret;
};
dojo.string.escape=function(type,str){
var args=dojo.lang.toArray(arguments,1);
switch(type.toLowerCase()){
case "xml":
case "html":
case "xhtml":
return dojo.string.escapeXml.apply(this,args);
case "sql":
return dojo.string.escapeSql.apply(this,args);
case "regexp":
case "regex":
return dojo.string.escapeRegExp.apply(this,args);
case "javascript":
case "jscript":
case "js":
return dojo.string.escapeJavaScript.apply(this,args);
case "ascii":
return dojo.string.encodeAscii.apply(this,args);
default:
return str;
}
};
dojo.string.escapeXml=function(str,_774){
str=str.replace(/&/gm,"&amp;").replace(/</gm,"&lt;").replace(/>/gm,"&gt;").replace(/"/gm,"&quot;");
if(!_774){
str=str.replace(/'/gm,"&#39;");
}
return str;
};
dojo.string.escapeSql=function(str){
return str.replace(/'/gm,"''");
};
dojo.string.escapeRegExp=function(str){
return str.replace(/\\/gm,"\\\\").replace(/([\f\b\n\t\r[\^$|?*+(){}])/gm,"\\$1");
};
dojo.string.escapeJavaScript=function(str){
return str.replace(/(["'\f\b\n\t\r])/gm,"\\$1");
};
dojo.string.escapeString=function(str){
return ("\""+str.replace(/(["\\])/g,"\\$1")+"\"").replace(/[\f]/g,"\\f").replace(/[\b]/g,"\\b").replace(/[\n]/g,"\\n").replace(/[\t]/g,"\\t").replace(/[\r]/g,"\\r");
};
dojo.string.summary=function(str,len){
if(!len||str.length<=len){
return str;
}
return str.substring(0,len).replace(/\.+$/,"")+"...";
};
dojo.string.endsWith=function(str,end,_77d){
if(_77d){
str=str.toLowerCase();
end=end.toLowerCase();
}
if((str.length-end.length)<0){
return false;
}
return str.lastIndexOf(end)==str.length-end.length;
};
dojo.string.endsWithAny=function(str){
for(var i=1;i<arguments.length;i++){
if(dojo.string.endsWith(str,arguments[i])){
return true;
}
}
return false;
};
dojo.string.startsWith=function(str,_781,_782){
if(_782){
str=str.toLowerCase();
_781=_781.toLowerCase();
}
return str.indexOf(_781)==0;
};
dojo.string.startsWithAny=function(str){
for(var i=1;i<arguments.length;i++){
if(dojo.string.startsWith(str,arguments[i])){
return true;
}
}
return false;
};
dojo.string.has=function(str){
for(var i=1;i<arguments.length;i++){
if(str.indexOf(arguments[i])>-1){
return true;
}
}
return false;
};
dojo.string.normalizeNewlines=function(text,_788){
if(_788=="\n"){
text=text.replace(/\r\n/g,"\n");
text=text.replace(/\r/g,"\n");
}else{
if(_788=="\r"){
text=text.replace(/\r\n/g,"\r");
text=text.replace(/\n/g,"\r");
}else{
text=text.replace(/([^\r])\n/g,"$1\r\n").replace(/\r([^\n])/g,"\r\n$1");
}
}
return text;
};
dojo.string.splitEscaped=function(str,_78a){
var _78b=[];
for(var i=0,_78d=0;i<str.length;i++){
if(str.charAt(i)=="\\"){
i++;
continue;
}
if(str.charAt(i)==_78a){
_78b.push(str.substring(_78d,i));
_78d=i+1;
}
}
_78b.push(str.substr(_78d));
return _78b;
};
dojo.provide("dojo.undo.browser");
try{
if((!djConfig["preventBackButtonFix"])&&(!dojo.hostenv.post_load_)){
document.write("<iframe style='border: 0px; width: 1px; height: 1px; position: absolute; bottom: 0px; right: 0px; visibility: visible;' name='djhistory' id='djhistory' src='"+(djConfig["dojoIframeHistoryUrl"]||dojo.hostenv.getBaseScriptUri()+"iframe_history.html")+"'></iframe>");
}
}
catch(e){
}
if(dojo.render.html.opera){
dojo.debug("Opera is not supported with dojo.undo.browser, so back/forward detection will not work.");
}
dojo.undo.browser={initialHref:(!dj_undef("window"))?window.location.href:"",initialHash:(!dj_undef("window"))?window.location.hash:"",moveForward:false,historyStack:[],forwardStack:[],historyIframe:null,bookmarkAnchor:null,locationTimer:null,setInitialState:function(args){
this.initialState=this._createState(this.initialHref,args,this.initialHash);
},addToHistory:function(args){
this.forwardStack=[];
var hash=null;
var url=null;
if(!this.historyIframe){
if(djConfig["useXDomain"]&&!djConfig["dojoIframeHistoryUrl"]){
dojo.debug("dojo.undo.browser: When using cross-domain Dojo builds,"+" please save iframe_history.html to your domain and set djConfig.dojoIframeHistoryUrl"+" to the path on your domain to iframe_history.html");
}
this.historyIframe=window.frames["djhistory"];
}
if(!this.bookmarkAnchor){
this.bookmarkAnchor=document.createElement("a");
dojo.body().appendChild(this.bookmarkAnchor);
this.bookmarkAnchor.style.display="none";
}
if(args["changeUrl"]){
hash="#"+((args["changeUrl"]!==true)?args["changeUrl"]:(new Date()).getTime());
if(this.historyStack.length==0&&this.initialState.urlHash==hash){
this.initialState=this._createState(url,args,hash);
return;
}else{
if(this.historyStack.length>0&&this.historyStack[this.historyStack.length-1].urlHash==hash){
this.historyStack[this.historyStack.length-1]=this._createState(url,args,hash);
return;
}
}
this.changingUrl=true;
setTimeout("window.location.href = '"+hash+"'; dojo.undo.browser.changingUrl = false;",1);
this.bookmarkAnchor.href=hash;
if(dojo.render.html.ie){
url=this._loadIframeHistory();
var _792=args["back"]||args["backButton"]||args["handle"];
var tcb=function(_794){
if(window.location.hash!=""){
setTimeout("window.location.href = '"+hash+"';",1);
}
_792.apply(this,[_794]);
};
if(args["back"]){
args.back=tcb;
}else{
if(args["backButton"]){
args.backButton=tcb;
}else{
if(args["handle"]){
args.handle=tcb;
}
}
}
var _795=args["forward"]||args["forwardButton"]||args["handle"];
var tfw=function(_797){
if(window.location.hash!=""){
window.location.href=hash;
}
if(_795){
_795.apply(this,[_797]);
}
};
if(args["forward"]){
args.forward=tfw;
}else{
if(args["forwardButton"]){
args.forwardButton=tfw;
}else{
if(args["handle"]){
args.handle=tfw;
}
}
}
}else{
if(dojo.render.html.moz){
if(!this.locationTimer){
this.locationTimer=setInterval("dojo.undo.browser.checkLocation();",200);
}
}
}
}else{
url=this._loadIframeHistory();
}
this.historyStack.push(this._createState(url,args,hash));
},checkLocation:function(){
if(!this.changingUrl){
var hsl=this.historyStack.length;
if((window.location.hash==this.initialHash||window.location.href==this.initialHref)&&(hsl==1)){
this.handleBackButton();
return;
}
if(this.forwardStack.length>0){
if(this.forwardStack[this.forwardStack.length-1].urlHash==window.location.hash){
this.handleForwardButton();
return;
}
}
if((hsl>=2)&&(this.historyStack[hsl-2])){
if(this.historyStack[hsl-2].urlHash==window.location.hash){
this.handleBackButton();
return;
}
}
}
},iframeLoaded:function(evt,_79a){
if(!dojo.render.html.opera){
var _79b=this._getUrlQuery(_79a.href);
if(_79b==null){
if(this.historyStack.length==1){
this.handleBackButton();
}
return;
}
if(this.moveForward){
this.moveForward=false;
return;
}
if(this.historyStack.length>=2&&_79b==this._getUrlQuery(this.historyStack[this.historyStack.length-2].url)){
this.handleBackButton();
}else{
if(this.forwardStack.length>0&&_79b==this._getUrlQuery(this.forwardStack[this.forwardStack.length-1].url)){
this.handleForwardButton();
}
}
}
},handleBackButton:function(){
var _79c=this.historyStack.pop();
if(!_79c){
return;
}
var last=this.historyStack[this.historyStack.length-1];
if(!last&&this.historyStack.length==0){
last=this.initialState;
}
if(last){
if(last.kwArgs["back"]){
last.kwArgs["back"]();
}else{
if(last.kwArgs["backButton"]){
last.kwArgs["backButton"]();
}else{
if(last.kwArgs["handle"]){
last.kwArgs.handle("back");
}
}
}
}
this.forwardStack.push(_79c);
},handleForwardButton:function(){
var last=this.forwardStack.pop();
if(!last){
return;
}
if(last.kwArgs["forward"]){
last.kwArgs.forward();
}else{
if(last.kwArgs["forwardButton"]){
last.kwArgs.forwardButton();
}else{
if(last.kwArgs["handle"]){
last.kwArgs.handle("forward");
}
}
}
this.historyStack.push(last);
},_createState:function(url,args,hash){
return {"url":url,"kwArgs":args,"urlHash":hash};
},_getUrlQuery:function(url){
var _7a3=url.split("?");
if(_7a3.length<2){
return null;
}else{
return _7a3[1];
}
},_loadIframeHistory:function(){
var url=(djConfig["dojoIframeHistoryUrl"]||dojo.hostenv.getBaseScriptUri()+"iframe_history.html")+"?"+(new Date()).getTime();
this.moveForward=true;
dojo.io.setIFrameSrc(this.historyIframe,url,false);
return url;
}};
dojo.provide("dojo.io.BrowserIO");
if(!dj_undef("window")){
dojo.io.checkChildrenForFile=function(node){
var _7a6=false;
var _7a7=node.getElementsByTagName("input");
dojo.lang.forEach(_7a7,function(_7a8){
if(_7a6){
return;
}
if(_7a8.getAttribute("type")=="file"){
_7a6=true;
}
});
return _7a6;
};
dojo.io.formHasFile=function(_7a9){
return dojo.io.checkChildrenForFile(_7a9);
};
dojo.io.updateNode=function(node,_7ab){
node=dojo.byId(node);
var args=_7ab;
if(dojo.lang.isString(_7ab)){
args={url:_7ab};
}
args.mimetype="text/html";
args.load=function(t,d,e){
while(node.firstChild){
dojo.dom.destroyNode(node.firstChild);
}
node.innerHTML=d;
};
dojo.io.bind(args);
};
dojo.io.formFilter=function(node){
var type=(node.type||"").toLowerCase();
return !node.disabled&&node.name&&!dojo.lang.inArray(["file","submit","image","reset","button"],type);
};
dojo.io.encodeForm=function(_7b2,_7b3,_7b4){
if((!_7b2)||(!_7b2.tagName)||(!_7b2.tagName.toLowerCase()=="form")){
dojo.raise("Attempted to encode a non-form element.");
}
if(!_7b4){
_7b4=dojo.io.formFilter;
}
var enc=/utf/i.test(_7b3||"")?encodeURIComponent:dojo.string.encodeAscii;
var _7b6=[];
for(var i=0;i<_7b2.elements.length;i++){
var elm=_7b2.elements[i];
if(!elm||elm.tagName.toLowerCase()=="fieldset"||!_7b4(elm)){
continue;
}
var name=enc(elm.name);
var type=elm.type.toLowerCase();
if(type=="select-multiple"){
for(var j=0;j<elm.options.length;j++){
if(elm.options[j].selected){
_7b6.push(name+"="+enc(elm.options[j].value));
}
}
}else{
if(dojo.lang.inArray(["radio","checkbox"],type)){
if(elm.checked){
_7b6.push(name+"="+enc(elm.value));
}
}else{
_7b6.push(name+"="+enc(elm.value));
}
}
}
var _7bc=_7b2.getElementsByTagName("input");
for(var i=0;i<_7bc.length;i++){
var _7bd=_7bc[i];
if(_7bd.type.toLowerCase()=="image"&&_7bd.form==_7b2&&_7b4(_7bd)){
var name=enc(_7bd.name);
_7b6.push(name+"="+enc(_7bd.value));
_7b6.push(name+".x=0");
_7b6.push(name+".y=0");
}
}
return _7b6.join("&")+"&";
};
dojo.io.FormBind=function(args){
this.bindArgs={};
if(args&&args.formNode){
this.init(args);
}else{
if(args){
this.init({formNode:args});
}
}
};
dojo.lang.extend(dojo.io.FormBind,{form:null,bindArgs:null,clickedButton:null,init:function(args){
var form=dojo.byId(args.formNode);
if(!form||!form.tagName||form.tagName.toLowerCase()!="form"){
throw new Error("FormBind: Couldn't apply, invalid form");
}else{
if(this.form==form){
return;
}else{
if(this.form){
throw new Error("FormBind: Already applied to a form");
}
}
}
dojo.lang.mixin(this.bindArgs,args);
this.form=form;
this.connect(form,"onsubmit","submit");
for(var i=0;i<form.elements.length;i++){
var node=form.elements[i];
if(node&&node.type&&dojo.lang.inArray(["submit","button"],node.type.toLowerCase())){
this.connect(node,"onclick","click");
}
}
var _7c3=form.getElementsByTagName("input");
for(var i=0;i<_7c3.length;i++){
var _7c4=_7c3[i];
if(_7c4.type.toLowerCase()=="image"&&_7c4.form==form){
this.connect(_7c4,"onclick","click");
}
}
},onSubmit:function(form){
return true;
},submit:function(e){
e.preventDefault();
if(this.onSubmit(this.form)){
dojo.io.bind(dojo.lang.mixin(this.bindArgs,{formFilter:dojo.lang.hitch(this,"formFilter")}));
}
},click:function(e){
var node=e.currentTarget;
if(node.disabled){
return;
}
this.clickedButton=node;
},formFilter:function(node){
var type=(node.type||"").toLowerCase();
var _7cb=false;
if(node.disabled||!node.name){
_7cb=false;
}else{
if(dojo.lang.inArray(["submit","button","image"],type)){
if(!this.clickedButton){
this.clickedButton=node;
}
_7cb=node==this.clickedButton;
}else{
_7cb=!dojo.lang.inArray(["file","submit","reset","button"],type);
}
}
return _7cb;
},connect:function(_7cc,_7cd,_7ce){
if(dojo.evalObjPath("dojo.event.connect")){
dojo.event.connect(_7cc,_7cd,this,_7ce);
}else{
var fcn=dojo.lang.hitch(this,_7ce);
_7cc[_7cd]=function(e){
if(!e){
e=window.event;
}
if(!e.currentTarget){
e.currentTarget=e.srcElement;
}
if(!e.preventDefault){
e.preventDefault=function(){
window.event.returnValue=false;
};
}
fcn(e);
};
}
}});
dojo.io.XMLHTTPTransport=new function(){
var _7d1=this;
var _7d2={};
this.useCache=false;
this.preventCache=false;
function getCacheKey(url,_7d4,_7d5){
return url+"|"+_7d4+"|"+_7d5.toLowerCase();
}
function addToCache(url,_7d7,_7d8,http){
_7d2[getCacheKey(url,_7d7,_7d8)]=http;
}
function getFromCache(url,_7db,_7dc){
return _7d2[getCacheKey(url,_7db,_7dc)];
}
this.clearCache=function(){
_7d2={};
};
function doLoad(_7dd,http,url,_7e0,_7e1){
if(((http.status>=200)&&(http.status<300))||(http.status==304)||(location.protocol=="file:"&&(http.status==0||http.status==undefined))||(location.protocol=="chrome:"&&(http.status==0||http.status==undefined))){
var ret;
if(_7dd.method.toLowerCase()=="head"){
var _7e3=http.getAllResponseHeaders();
ret={};
ret.toString=function(){
return _7e3;
};
var _7e4=_7e3.split(/[\r\n]+/g);
for(var i=0;i<_7e4.length;i++){
var pair=_7e4[i].match(/^([^:]+)\s*:\s*(.+)$/i);
if(pair){
ret[pair[1]]=pair[2];
}
}
}else{
if(_7dd.mimetype=="text/javascript"){
try{
ret=dj_eval(http.responseText);
}
catch(e){
dojo.debug(e);
dojo.debug(http.responseText);
ret=null;
}
}else{
if(_7dd.mimetype=="text/json"||_7dd.mimetype=="application/json"){
try{
ret=dj_eval("("+http.responseText+")");
}
catch(e){
dojo.debug(e);
dojo.debug(http.responseText);
ret=false;
}
}else{
if((_7dd.mimetype=="application/xml")||(_7dd.mimetype=="text/xml")){
ret=http.responseXML;
if(!ret||typeof ret=="string"||!http.getResponseHeader("Content-Type")){
ret=dojo.dom.createDocumentFromText(http.responseText);
}
}else{
ret=http.responseText;
}
}
}
}
if(_7e1){
addToCache(url,_7e0,_7dd.method,http);
}
_7dd[(typeof _7dd.load=="function")?"load":"handle"]("load",ret,http,_7dd);
}else{
var _7e7=new dojo.io.Error("XMLHttpTransport Error: "+http.status+" "+http.statusText);
_7dd[(typeof _7dd.error=="function")?"error":"handle"]("error",_7e7,http,_7dd);
}
}
function setHeaders(http,_7e9){
if(_7e9["headers"]){
for(var _7ea in _7e9["headers"]){
if(_7ea.toLowerCase()=="content-type"&&!_7e9["contentType"]){
_7e9["contentType"]=_7e9["headers"][_7ea];
}else{
http.setRequestHeader(_7ea,_7e9["headers"][_7ea]);
}
}
}
}
this.inFlight=[];
this.inFlightTimer=null;
this.startWatchingInFlight=function(){
if(!this.inFlightTimer){
this.inFlightTimer=setTimeout("dojo.io.XMLHTTPTransport.watchInFlight();",10);
}
};
this.watchInFlight=function(){
var now=null;
if(!dojo.hostenv._blockAsync&&!_7d1._blockAsync){
for(var x=this.inFlight.length-1;x>=0;x--){
try{
var tif=this.inFlight[x];
if(!tif||tif.http._aborted||!tif.http.readyState){
this.inFlight.splice(x,1);
continue;
}
if(4==tif.http.readyState){
this.inFlight.splice(x,1);
doLoad(tif.req,tif.http,tif.url,tif.query,tif.useCache);
}else{
if(tif.startTime){
if(!now){
now=(new Date()).getTime();
}
if(tif.startTime+(tif.req.timeoutSeconds*1000)<now){
if(typeof tif.http.abort=="function"){
tif.http.abort();
}
this.inFlight.splice(x,1);
tif.req[(typeof tif.req.timeout=="function")?"timeout":"handle"]("timeout",null,tif.http,tif.req);
}
}
}
}
catch(e){
try{
var _7ee=new dojo.io.Error("XMLHttpTransport.watchInFlight Error: "+e);
tif.req[(typeof tif.req.error=="function")?"error":"handle"]("error",_7ee,tif.http,tif.req);
}
catch(e2){
dojo.debug("XMLHttpTransport error callback failed: "+e2);
}
}
}
}
clearTimeout(this.inFlightTimer);
if(this.inFlight.length==0){
this.inFlightTimer=null;
return;
}
this.inFlightTimer=setTimeout("dojo.io.XMLHTTPTransport.watchInFlight();",10);
};
var _7ef=dojo.hostenv.getXmlhttpObject()?true:false;
this.canHandle=function(_7f0){
return _7ef&&dojo.lang.inArray(["text/plain","text/html","application/xml","text/xml","text/javascript","text/json","application/json"],(_7f0["mimetype"].toLowerCase()||""))&&!(_7f0["formNode"]&&dojo.io.formHasFile(_7f0["formNode"]));
};
this.multipartBoundary="45309FFF-BD65-4d50-99C9-36986896A96F";
this.bind=function(_7f1){
if(!_7f1["url"]){
if(!_7f1["formNode"]&&(_7f1["backButton"]||_7f1["back"]||_7f1["changeUrl"]||_7f1["watchForURL"])&&(!djConfig.preventBackButtonFix)){
dojo.deprecated("Using dojo.io.XMLHTTPTransport.bind() to add to browser history without doing an IO request","Use dojo.undo.browser.addToHistory() instead.","0.4");
dojo.undo.browser.addToHistory(_7f1);
return true;
}
}
var url=_7f1.url;
var _7f3="";
if(_7f1["formNode"]){
var ta=_7f1.formNode.getAttribute("action");
if((ta)&&(!_7f1["url"])){
url=ta;
}
var tp=_7f1.formNode.getAttribute("method");
if((tp)&&(!_7f1["method"])){
_7f1.method=tp;
}
_7f3+=dojo.io.encodeForm(_7f1.formNode,_7f1.encoding,_7f1["formFilter"]);
}
if(url.indexOf("#")>-1){
dojo.debug("Warning: dojo.io.bind: stripping hash values from url:",url);
url=url.split("#")[0];
}
if(_7f1["file"]){
_7f1.method="post";
}
if(!_7f1["method"]){
_7f1.method="get";
}
if(_7f1.method.toLowerCase()=="get"){
_7f1.multipart=false;
}else{
if(_7f1["file"]){
_7f1.multipart=true;
}else{
if(!_7f1["multipart"]){
_7f1.multipart=false;
}
}
}
if(_7f1["backButton"]||_7f1["back"]||_7f1["changeUrl"]){
dojo.undo.browser.addToHistory(_7f1);
}
var _7f6=_7f1["content"]||{};
if(_7f1.sendTransport){
_7f6["dojo.transport"]="xmlhttp";
}
do{
if(_7f1.postContent){
_7f3=_7f1.postContent;
break;
}
if(_7f6){
_7f3+=dojo.io.argsFromMap(_7f6,_7f1.encoding);
}
if(_7f1.method.toLowerCase()=="get"||!_7f1.multipart){
break;
}
var t=[];
if(_7f3.length){
var q=_7f3.split("&");
for(var i=0;i<q.length;++i){
if(q[i].length){
var p=q[i].split("=");
t.push("--"+this.multipartBoundary,"Content-Disposition: form-data; name=\""+p[0]+"\"","",p[1]);
}
}
}
if(_7f1.file){
if(dojo.lang.isArray(_7f1.file)){
for(var i=0;i<_7f1.file.length;++i){
var o=_7f1.file[i];
t.push("--"+this.multipartBoundary,"Content-Disposition: form-data; name=\""+o.name+"\"; filename=\""+("fileName" in o?o.fileName:o.name)+"\"","Content-Type: "+("contentType" in o?o.contentType:"application/octet-stream"),"",o.content);
}
}else{
var o=_7f1.file;
t.push("--"+this.multipartBoundary,"Content-Disposition: form-data; name=\""+o.name+"\"; filename=\""+("fileName" in o?o.fileName:o.name)+"\"","Content-Type: "+("contentType" in o?o.contentType:"application/octet-stream"),"",o.content);
}
}
if(t.length){
t.push("--"+this.multipartBoundary+"--","");
_7f3=t.join("\r\n");
}
}while(false);
var _7fc=_7f1["sync"]?false:true;
var _7fd=_7f1["preventCache"]||(this.preventCache==true&&_7f1["preventCache"]!=false);
var _7fe=_7f1["useCache"]==true||(this.useCache==true&&_7f1["useCache"]!=false);
if(!_7fd&&_7fe){
var _7ff=getFromCache(url,_7f3,_7f1.method);
if(_7ff){
doLoad(_7f1,_7ff,url,_7f3,false);
return;
}
}
var http=dojo.hostenv.getXmlhttpObject(_7f1);
var _801=false;
if(_7fc){
var _802=this.inFlight.push({"req":_7f1,"http":http,"url":url,"query":_7f3,"useCache":_7fe,"startTime":_7f1.timeoutSeconds?(new Date()).getTime():0});
this.startWatchingInFlight();
}else{
_7d1._blockAsync=true;
}
if(_7f1.method.toLowerCase()=="post"){
if(!_7f1.user){
http.open("POST",url,_7fc);
}else{
http.open("POST",url,_7fc,_7f1.user,_7f1.password);
}
setHeaders(http,_7f1);
http.setRequestHeader("Content-Type",_7f1.multipart?("multipart/form-data; boundary="+this.multipartBoundary):(_7f1.contentType||"application/x-www-form-urlencoded"));
try{
http.send(_7f3);
}
catch(e){
if(typeof http.abort=="function"){
http.abort();
}
doLoad(_7f1,{status:404},url,_7f3,_7fe);
}
}else{
var _803=url;
if(_7f3!=""){
_803+=(_803.indexOf("?")>-1?"&":"?")+_7f3;
}
if(_7fd){
_803+=(dojo.string.endsWithAny(_803,"?","&")?"":(_803.indexOf("?")>-1?"&":"?"))+"dojo.preventCache="+new Date().valueOf();
}
if(!_7f1.user){
http.open(_7f1.method.toUpperCase(),_803,_7fc);
}else{
http.open(_7f1.method.toUpperCase(),_803,_7fc,_7f1.user,_7f1.password);
}
setHeaders(http,_7f1);
try{
http.send(null);
}
catch(e){
if(typeof http.abort=="function"){
http.abort();
}
doLoad(_7f1,{status:404},url,_7f3,_7fe);
}
}
if(!_7fc){
doLoad(_7f1,http,url,_7f3,_7fe);
_7d1._blockAsync=false;
}
_7f1.abort=function(){
try{
http._aborted=true;
}
catch(e){
}
return http.abort();
};
return;
};
dojo.io.transports.addTransport("XMLHTTPTransport");
};
}
dojo.provide("dojo.io.cookie");
dojo.io.cookie.setCookie=function(name,_805,days,path,_808,_809){
var _80a=-1;
if((typeof days=="number")&&(days>=0)){
var d=new Date();
d.setTime(d.getTime()+(days*24*60*60*1000));
_80a=d.toGMTString();
}
_805=escape(_805);
document.cookie=name+"="+_805+";"+(_80a!=-1?" expires="+_80a+";":"")+(path?"path="+path:"")+(_808?"; domain="+_808:"")+(_809?"; secure":"");
};
dojo.io.cookie.set=dojo.io.cookie.setCookie;
dojo.io.cookie.getCookie=function(name){
var idx=document.cookie.lastIndexOf(name+"=");
if(idx==-1){
return null;
}
var _80e=document.cookie.substring(idx+name.length+1);
var end=_80e.indexOf(";");
if(end==-1){
end=_80e.length;
}
_80e=_80e.substring(0,end);
_80e=unescape(_80e);
return _80e;
};
dojo.io.cookie.get=dojo.io.cookie.getCookie;
dojo.io.cookie.deleteCookie=function(name){
dojo.io.cookie.setCookie(name,"-",0);
};
dojo.io.cookie.setObjectCookie=function(name,obj,days,path,_815,_816,_817){
if(arguments.length==5){
_817=_815;
_815=null;
_816=null;
}
var _818=[],_819,_81a="";
if(!_817){
_819=dojo.io.cookie.getObjectCookie(name);
}
if(days>=0){
if(!_819){
_819={};
}
for(var prop in obj){
if(obj[prop]==null){
delete _819[prop];
}else{
if((typeof obj[prop]=="string")||(typeof obj[prop]=="number")){
_819[prop]=obj[prop];
}
}
}
prop=null;
for(var prop in _819){
_818.push(escape(prop)+"="+escape(_819[prop]));
}
_81a=_818.join("&");
}
dojo.io.cookie.setCookie(name,_81a,days,path,_815,_816);
};
dojo.io.cookie.getObjectCookie=function(name){
var _81d=null,_81e=dojo.io.cookie.getCookie(name);
if(_81e){
_81d={};
var _81f=_81e.split("&");
for(var i=0;i<_81f.length;i++){
var pair=_81f[i].split("=");
var _822=pair[1];
if(isNaN(_822)){
_822=unescape(pair[1]);
}
_81d[unescape(pair[0])]=_822;
}
}
return _81d;
};
dojo.io.cookie.isSupported=function(){
if(typeof navigator.cookieEnabled!="boolean"){
dojo.io.cookie.setCookie("__TestingYourBrowserForCookieSupport__","CookiesAllowed",90,null);
var _823=dojo.io.cookie.getCookie("__TestingYourBrowserForCookieSupport__");
navigator.cookieEnabled=(_823=="CookiesAllowed");
if(navigator.cookieEnabled){
this.deleteCookie("__TestingYourBrowserForCookieSupport__");
}
}
return navigator.cookieEnabled;
};
if(!dojo.io.cookies){
dojo.io.cookies=dojo.io.cookie;
}
dojo.kwCompoundRequire({common:["dojo.io.common"],rhino:["dojo.io.RhinoIO"],browser:["dojo.io.BrowserIO","dojo.io.cookie"],dashboard:["dojo.io.BrowserIO","dojo.io.cookie"]});
dojo.provide("dojo.io.*");

