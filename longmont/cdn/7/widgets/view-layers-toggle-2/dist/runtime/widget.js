System.register(["jimu-core","esri/layers/FeatureLayer","jimu-arcgis"],(function(e,t){var a={},s={},r={};return{setters:[function(e){a.React=e.React,a.jsx=e.jsx},function(e){s.default=e.default},function(e){r.JimuMapView=e.JimuMapView,r.JimuMapViewComponent=e.JimuMapViewComponent}],execute:function(){e((()=>{var e={818:e=>{"use strict";e.exports=s},826:e=>{"use strict";e.exports=r},891:e=>{"use strict";e.exports=a}},t={};function i(a){var s=t[a];if(void 0!==s)return s.exports;var r=t[a]={exports:{}};return e[a](r,r.exports,i),r.exports}i.d=(e,t)=>{for(var a in t)i.o(t,a)&&!i.o(e,a)&&Object.defineProperty(e,a,{enumerable:!0,get:t[a]})},i.o=(e,t)=>Object.prototype.hasOwnProperty.call(e,t),i.r=e=>{"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},i.p="";var o={};return i.p=window.jimuConfig.baseUrl,(()=>{"use strict";i.r(o),i.d(o,{default:()=>s});var e=i(891);var t=i(818),a=i(826);class s extends e.React.PureComponent{constructor(e){super(e),this.selectChangeHandler=e=>{if(this.state.jimuMapView){if(this.state.featureLayerOnMap&&(this.state.jimuMapView.view.map.remove(this.state.featureLayerOnMap),this.setState({featureLayerOnMap:void 0})),e.target.value&&""!==e.target.value){const a=new t.default({url:e.target.value});this.state.jimuMapView.view.map.add(a,1),this.setState({featureLayerOnMap:a})}}else console.error("You probably need to choose you map in the settings panel.")},this.state={jimuMapView:a.JimuMapView,featureLayerOnMap:void 0}}render(){var t;return(0,e.jsx)("div",{className:"widget-view-layers-toggle jimu-widget",style:{overflow:"auto"}},this.props.hasOwnProperty("useMapWidgetIds")&&this.props.useMapWidgetIds&&1===this.props.useMapWidgetIds.length&&(0,e.jsx)(a.JimuMapViewComponent,{useMapWidgetId:null===(t=this.props.useMapWidgetIds)||void 0===t?void 0:t[0],onActiveViewChange:e=>{this.setState({jimuMapView:e})}}),(0,e.jsx)("p",{className:"shadow-lg m-3 p-3 bg-white rounded"},"View Accessible Area",":",(0,e.jsx)("select",{onChange:e=>{this.selectChangeHandler(e)},style:{maxWidth:"100%"}},(0,e.jsx)("option",{value:""}),this.props.config.layerUrls.map((t=>(0,e.jsx)("option",{value:"https://services1.arcgis.com/Sb0rgSd67ecLKIvl/arcgis/rest/services/".concat(t.replace(/ /g,"_"),"/FeatureServer")},t))))))}}})(),o})())}}}));