System.register(["jimu-core","esri/layers/FeatureLayer","jimu-arcgis"],(function(e,t){var s={},r={},a={};return{setters:[function(e){s.React=e.React,s.jsx=e.jsx},function(e){r.default=e.default},function(e){a.JimuMapViewComponent=e.JimuMapViewComponent}],execute:function(){e((()=>{var e={818:e=>{"use strict";e.exports=r},826:e=>{"use strict";e.exports=a},891:e=>{"use strict";e.exports=s}},t={};function i(s){var r=t[s];if(void 0!==r)return r.exports;var a=t[s]={exports:{}};return e[s](a,a.exports,i),a.exports}i.d=(e,t)=>{for(var s in t)i.o(t,s)&&!i.o(e,s)&&Object.defineProperty(e,s,{enumerable:!0,get:t[s]})},i.o=(e,t)=>Object.prototype.hasOwnProperty.call(e,t),i.r=e=>{"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},i.p="";var o={};return i.p=window.jimuConfig.baseUrl,(()=>{"use strict";i.r(o),i.d(o,{default:()=>r});var e=i(891);var t=i(818),s=i(826);class r extends e.React.PureComponent{constructor(e){super(e),this.selectChangeHandler=e=>{if(this.state.jimuMapView){if(this.state.featureLayerOnMap&&(this.state.jimuMapView.view.map.remove(this.state.featureLayerOnMap),this.setState({featureLayerOnMap:void 0})),e.target.value&&""!==e.target.value){const s=new t.default({url:e.target.value});this.state.jimuMapView.view.map.add(s,0),this.setState({featureLayerOnMap:s})}}else console.error("You probably need to choose your map in the settings panel.")},this.state={jimuMapView:void 0,featureLayerOnMap:void 0}}render(){var t;const r=this.props.config.layerUrls&&this.props.config.layerUrls.length>0,a="https://services1.arcgis.com/Sb0rgSd67ecLKIvl/arcgis/rest/services/Not_Enrolled_aged_3_to_4/FeatureServer";return(0,e.jsx)("div",{className:"widget-view-layers-toggle jimu-widget",style:{overflow:"auto"}},this.props.hasOwnProperty("useMapWidgetIds")&&this.props.useMapWidgetIds&&1===this.props.useMapWidgetIds.length&&(0,e.jsx)(s.JimuMapViewComponent,{useMapWidgetId:null===(t=this.props.useMapWidgetIds)||void 0===t?void 0:t[0],onActiveViewChange:e=>{this.setState({jimuMapView:e},(()=>{r&&this.selectChangeHandler({target:{value:a}})}))}}),(0,e.jsx)("p",{className:"shadow-lg m-3 p-3 bg-white rounded"},"View Census Data",":",r?(0,e.jsx)("select",{defaultValue:a,onChange:e=>{this.selectChangeHandler(e)},style:{maxWidth:"100%"}},this.props.config.layerUrls.map((t=>(0,e.jsx)("option",{value:"https://services1.arcgis.com/Sb0rgSd67ecLKIvl/arcgis/rest/services/".concat(t.replace(/ /g,"_"),"/FeatureServer")},t)))):(0,e.jsx)("span",null,"No layer URLs available.")))}}})(),o})())}}}));