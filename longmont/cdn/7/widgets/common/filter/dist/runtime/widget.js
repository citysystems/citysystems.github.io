System.register(["jimu-core","jimu-ui","jimu-ui/basic/sql-expression-runtime"],(function(e,t){var s={},i={},r={};return{setters:[function(e){s.BaseVersionManager=e.BaseVersionManager,s.DataSourceComponent=e.DataSourceComponent,s.DataSourceFilterChangeMessage=e.DataSourceFilterChangeMessage,s.DataSourceManager=e.DataSourceManager,s.DataSourceStatus=e.DataSourceStatus,s.Immutable=e.Immutable,s.MessageManager=e.MessageManager,s.React=e.React,s.appConfigUtils=e.appConfigUtils,s.classNames=e.classNames,s.css=e.css,s.dataSourceUtils=e.dataSourceUtils,s.defaultMessages=e.defaultMessages,s.getAppStore=e.getAppStore,s.jsx=e.jsx,s.lodash=e.lodash,s.polished=e.polished},function(e){i.Alert=e.Alert,i.Badge=e.Badge,i.Button=e.Button,i.Card=e.Card,i.Icon=e.Icon,i.Popper=e.Popper,i.Switch=e.Switch,i.WidgetPlaceholder=e.WidgetPlaceholder,i.applyMaxSizeModifier=e.applyMaxSizeModifier,i.defaultMessages=e.defaultMessages,i.getCustomFlipModifier=e.getCustomFlipModifier,i.maxSizeModifier=e.maxSizeModifier},function(e){r.SqlExpressionRuntime=e.SqlExpressionRuntime,r.getShownClauseNumberByExpression=e.getShownClauseNumberByExpression,r.getTotalClauseNumberByExpression=e.getTotalClauseNumberByExpression,r.updateSQLExpressionByVersion=e.updateSQLExpressionByVersion}],execute:function(){e((()=>{var e={33679:e=>{e.exports='<svg viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg"><g fill="none" fill-rule="evenodd"><path d="M2.46.5a.44.44 0 0 0-.283.105.52.52 0 0 0-.068.709l4.783 6.098c.184.235.294.52.317.819l.836 10.814a.525.525 0 0 0 .03.139c.091.253.353.377.585.286l2.112-.82a.49.49 0 0 0 .3-.397l1.325-10.061c.038-.288.157-.558.342-.779l5.141-6.085c.077-.091.12-.21.12-.333C18 .718 17.79.5 17.54.5H2.46Z" stroke="#000"></path><path d="M0 0h20v20H0z"></path></g></svg>'},10119:e=>{e.exports='<svg viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="m14 4-6 8-6-8h12Z" fill="#000"></path></svg>'},27218:e=>{e.exports='<svg viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M14.938 8A7 7 0 0 1 1.01 9H0a8.001 8.001 0 0 0 15.938-1A8 8 0 0 0 1.02 3.98L1 .702a.5.5 0 1 0-1 .006L.031 5.9l5.128-.826a.5.5 0 0 0-.16-.987L1.819 4.6A7 7 0 0 1 14.938 8Z" fill="#000"></path></svg>'},48891:e=>{"use strict";e.exports=s},30726:e=>{"use strict";e.exports=i},15506:e=>{"use strict";e.exports=r}},t={};function a(s){var i=t[s];if(void 0!==i)return i.exports;var r=t[s]={exports:{}};return e[s](r,r.exports,a),r.exports}a.n=e=>{var t=e&&e.__esModule?()=>e.default:()=>e;return a.d(t,{a:t}),t},a.d=(e,t)=>{for(var s in t)a.o(t,s)&&!a.o(e,s)&&Object.defineProperty(e,s,{enumerable:!0,get:t[s]})},a.o=(e,t)=>Object.prototype.hasOwnProperty.call(e,t),a.r=e=>{"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},a.p="";var o={};return a.p=window.jimuConfig.baseUrl,(()=>{"use strict";a.r(o),a.d(o,{default:()=>v});var e,t,s=a(48891);!function(e){e.Block="BLOCK",e.Inline="INLINE",e.Popper="POPPER"}(e||(e={})),function(e){e.Toggle="TOGGLE",e.Button="BUTTON"}(t||(t={}));var i=a(30726),r=a(15506);function l(e){const t=(0,s.getAppStore)().getState().appContext.isRTL,i="200px",r="300px",a="350px";return s.css`
    .filter-items-container, &.filter-items-container {
      overflow: auto;

      .filter-item {
        padding-bottom: 0.5rem;

        &.filter-item-popper{
          margin: 0.5rem;
          min-width: ${r};
          max-width: ${a};
        }

        .filter-item-inline {
          padding-bottom: 0.5rem;
          padding-top: 0.5rem;

          .filter-item-arrow{
            transform: rotate(${t?90:270}deg);
          }
          .filter-item-icon{
            margin-right: 0.5rem;

            &.no-arrow{
              margin-left: 0.5rem;
            }
          }
          .filter-item-name{
            font-size: ${s.polished.rem(13)};
            color: ${e.colors.black};
            word-break: break-word;
            &.no-icons{
              margin-left: 0.5rem;
            }
            &.toggle-name{
              white-space: nowrap;
              margin-right: 0.5rem;
            }
          }

          /* sql-expression-styles - start */
          .sql-expression-inline{
            align-items: center;

            &.sql-expression-wrap{
              display: block !important;

              .sql-expression-builder{
                overflow-x: hidden;
                .sql-expression-container{
                  flex-wrap: wrap;
                  align-content: flex-start;

                  .sql-expression-set{
                    flex-wrap: wrap;
                  }
                }
              }

            }

            .sql-expression-builder{
              overflow-x: auto;
              .sql-expression-container{
                display: flex;
                .sql-expression-single{
                  margin-right: 0.5rem;
                  &:last-of-type{
                    margin-right: 0;
                  }
                  /* .clause-inline{
                    min-width: ${i};
                  }
                  .clause-block{
                    .sql-expression-input{
                      min-width: ${i};
                    }
                  }
                  .sql-expression-displaylabel{
                    min-width: ${i};
                  } */
                }
                .sql-expression-set{
                  display: flex;
                }
              }
            }

          }
          /* sql-expression-styles - end */

        }
      }

      .filter-item:last-child{
        padding-bottom: 0 !important;
      }

      &.filter-items-inline{
        display: flex;
        .sql-expression-builder .sql-expression-container .sql-expression-single .sql-expression-input .pill-btn-container{
          flex-wrap: nowrap;
          .pill-btn{
            overflow: visible;
          }
        }

        &.filter-items-wrap{
          flex-wrap: wrap;
          align-content: flex-start;

          .sql-expression-builder .sql-expression-container .sql-expression-single .sql-expression-input .pill-btn-container{
            flex-wrap: wrap;
          }
        }
        .filter-item{
          /* padding: 0; */
          &.filter-item-popper{
            min-width: 300px;
            padding-bottom: 0.5rem;
            .filter-item-inline {
              padding-bottom: 0.5rem;
              padding-top: 0.5rem;
            }
          }
          .filter-item-inline{
            padding: 0;
            /* height: 100%; */
            overflow-y: auto;
            background-color: unset !important;
            border: none !important;

            .filter-expanded-container{
              width: ${r};
              padding-top: 0.5rem;
            }

            /* .filter-item-clause-pill{
              margin: 10px 5px;
              white-space: nowrap;
            } */

            /* .filter-popper-container{ */
              .filter-item-pill{
                margin: 10px 4px;
                white-space: nowrap;

                .sql-expression-single{
                  margin: 0;
                }

                &.filter-item-toggle-pill{
                  /* &:hover{
                    background-color: ${e.colors.palette.light[100]};
                  } */
                  display: flex;
                  flex-direction: row;
                  height: 32px;
                  align-items: center;
                  padding: 0 0.5rem;
                }
              /* } */
              /* .pill-display-label{
                white-space: nowrap;
                text-overflow: ellipsis;
                overflow: hidden;
              } */
            }

            /*input editor width*/
            .sql-expression-builder{
              .sql-expression-container{
                .sql-expression-single{
                  .clause-inline{
                    .sql-expression-label{
                      margin-right: 0.5rem;
                      width: auto;
                      overflow: visible;
                    }
                    .sql-expression-input{
                      width: auto;
                    }

                  }
                  /* .clause-block{ */
                    .sql-expression-input{
                      min-width: ${i};
                      .double-number-picker{
                        min-width: ${r};
                      }
                      .double-datetime-picker{
                        min-width: ${a};
                      }
                    }
                  /* } */
                  .sql-expression-displaylabel{
                    white-space: nowrap;
                    padding-right: 0.5rem;
                    font-size: 13px;
                  }
                }
              }
            }

          }
        }
      }

      &.filter-items-popup{
        min-width: ${r};
        max-width: ${a};
      }

      .apply-cancel-group{
        white-space: nowrap;
        overflow: visible;
      }
    }

    .filter-reset-container{
      display: flex;
      &.bottom-reset {
        margin-top: 0.5rem;
        justify-content: flex-end;
      }
      &.right-reset {
        height: fit-content;
        margin-top: 10px;
        margin-left: 0.5rem;
        margin-right: 0.25rem;
      }
    }
  `}var n=a(10119),p=a.n(n);const u=e=>{const t=window.SVG,{className:i}=e,r=function(e,t){var s={};for(var i in e)Object.prototype.hasOwnProperty.call(e,i)&&t.indexOf(i)<0&&(s[i]=e[i]);if(null!=e&&"function"==typeof Object.getOwnPropertySymbols){var r=0;for(i=Object.getOwnPropertySymbols(e);r<i.length;r++)t.indexOf(i[r])<0&&Object.prototype.propertyIsEnumerable.call(e,i[r])&&(s[i[r]]=e[i[r]])}return s}(e,["className"]),a=(0,s.classNames)("jimu-icon jimu-icon-component",i);return t?s.React.createElement(t,Object.assign({className:a,src:p()},r)):s.React.createElement("svg",Object.assign({className:a},r))},c=[{name:"preventOverflow",options:{altAxis:!0}}];class d extends s.React.PureComponent{constructor(a){super(a),this.getOutPutWidgetLabel=()=>{var e;return null===(e=(0,s.getAppStore)().getState().appConfig.widgets[s.appConfigUtils.getWidgetIdByOutputDataSource(this.props.useDataSource)])||void 0===e?void 0:e.label},this.getAppliedState=()=>{let e=this.props.config.autoApplyWhenWidgetOpen;return this.props.omitInternalStyle&&1===this.endUserClausesNum&&1===this.clausesNumConfigured&&(e=!0),e},this.oncollapsedChange=()=>{this.setState({collapsed:!this.state.collapsed})},this.onApplyChange=e=>{this.setState({sqlChanged:!1}),this.props.onChange(this.props.id,this.props.selectedDs,this.state.sqlExprObj,e)},this.onToggleChange=e=>{this.setState({applied:e}),this.onApplyChange(e)},this.onPillClick=(e,t)=>{if(e)this.setState({popperVersion:this.state.isOpen?this.state.popperVersion:this.state.popperVersion+1}),this.onTogglePopper();else{const e=t.className.indexOf("active")<0;this.onToggleChange(!!e)}},this.onSqlExpressionChange=e=>{var s;this.setState({sqlExprObj:e,sqlChanged:!(this.props.triggerType!==t.Button||this.props.omitInternalStyle||(null===(s=this.props.config.sqlExprObj)||void 0===s?void 0:s.sql)===(null==e?void 0:e.sql))}),(this.props.triggerType===t.Toggle||this.props.omitInternalStyle)&&this.props.onChange(this.props.id,this.props.selectedDs,e,this.state.applied)},this.onTogglePopper=()=>{this.state.isOpen&&s.lodash.defer((()=>{this.pillButton.focus()})),this.setState({isOpen:!this.state.isOpen})},this.getFilterItem=(r,a=!1)=>{const{icon:o,name:l}=this.props.config;return(0,s.jsx)("div",{className:"h-100"},(0,s.jsx)("div",{className:(0,s.classNames)("d-flex justify-content-between w-100 pr-2 align-items-center",a?"flex-row-reverse":"")},!a&&r&&(0,s.jsx)(i.Button,{size:"sm",icon:!0,type:"tertiary",onClick:this.oncollapsedChange,className:"jimu-outline-inside"},(0,s.jsx)(u,{className:this.state.collapsed?"filter-item-arrow":"",size:"s"})),!a&&o&&(0,s.jsx)("div",{className:(0,s.classNames)("filter-item-icon",r?"":"no-arrow")},(0,s.jsx)(i.Icon,{icon:o.svg,size:o.properties.size})),!a&&(0,s.jsx)("div",{className:(0,s.classNames)("filter-item-name flex-grow-1",r||o?"":"no-icons")},l),this.props.triggerType===t.Toggle&&(0,s.jsx)("div",{className:"ml-1 d-flex align-items-center"},this.getToggle())),this.state.sqlExprObj&&(0,s.jsx)("div",{style:{display:this.state.collapsed?"none":"block"},className:(0,s.classNames)("w-100 pl-5 pr-5",this.props.arrangeType===e.Inline&&1===this.props.filterNum&&this.props.omitInternalStyle?"sql-expression-inline":"",this.props.arrangeType===e.Inline&&1===this.props.filterNum&&this.props.wrap?"sql-expression-wrap":"")},this.getSqlExpression()),this.props.triggerType===t.Button&&(0,s.jsx)("div",{className:"d-flex justify-content-end pl-4 pr-4 pt-2 pb-2"},this.getApplyButtons()))},this.isDataSourceError=()=>null===this.props.selectedDs,this.isOutputFromWidget=()=>{var e;return null===(e=this.props.selectedDs)||void 0===e?void 0:e.getDataSourceJson().isOutputFromWidget},this.isOutputDataSourceValid=()=>this.isOutputFromWidget()&&!this.props.isNotReadyFromWidget,this.isOutputDataSourceInvalid=()=>this.isOutputFromWidget()&&this.props.isNotReadyFromWidget,this.isDataSourceValid=()=>this.props.selectedDs&&(this.isOutputFromWidget()&&!this.props.isNotReadyFromWidget||!this.isOutputDataSourceInvalid()),this.isDataSourceLoadingOrInvalid=()=>!this.isDataSourceValid(),this.getErrorIcon=()=>{if(this.isDataSourceError()){const e=this.props.intl.formatMessage({id:"dataSourceCreateError",defaultMessage:s.defaultMessages.dataSourceCreateError});return(0,s.jsx)(i.Alert,{buttonType:"tertiary",form:"tooltip",size:"small",type:"error",text:e})}if(this.isOutputDataSourceInvalid()){const e=this.props.intl.formatMessage({id:"outputDataIsNotGenerated",defaultMessage:i.defaultMessages.outputDataIsNotGenerated},{outputDsLabel:this.props.selectedDs.getLabel(),sourceWidgetName:this.state.outputWidgetLabel});return(0,s.jsx)(i.Alert,{buttonType:"tertiary",form:"tooltip",size:"small",type:"warning",text:e})}return null},this.getToggle=()=>(0,s.jsx)(s.React.Fragment,null,this.getErrorIcon(),(0,s.jsx)(i.Switch,{checked:this.state.applied,disabled:this.isDataSourceLoadingOrInvalid(),"aria-label":this.props.config.name,onChange:e=>this.onToggleChange(e.target.checked)})),this.getApplyButtons=()=>(0,s.jsx)("div",{className:"w-100 d-flex justify-content-end apply-cancel-group"},this.getErrorIcon(),(0,s.jsx)(i.Button,{type:"primary",className:"filter-apply-button wrap",disabled:this.isDataSourceLoadingOrInvalid()||!(!this.state.applied||this.state.sqlChanged),onClick:()=>this.onApplyChange(!0)},this.props.intl.formatMessage({id:"apply",defaultMessage:s.defaultMessages.apply})),(0,s.jsx)(i.Button,{type:"default",className:"filter-cancel-button ml-2",disabled:this.isDataSourceLoadingOrInvalid()||!this.state.applied,onClick:()=>this.onApplyChange(!1)},this.props.intl.formatMessage({id:"cancel",defaultMessage:s.defaultMessages.cancel}))),this.getTriggerNodeForClauses=(e=this.props.triggerType)=>{let s=null;switch(e){case t.Toggle:s=this.getToggle();break;case t.Button:s=this.getApplyButtons()}return s},this.getSqlExpression=()=>this.isDataSourceValid()?(0,s.jsx)(r.SqlExpressionRuntime,{widgetId:this.props.widgetId,dataSource:this.props.selectedDs,expression:this.state.sqlExprObj,onChange:this.onSqlExpressionChange}):null,this.getTirggerNodeForWrapClauses=e=>e===this.props.triggerType&&this.isSingleFilterAndMultipleClauses()&&this.props.wrap&&(0,s.jsx)("div",{className:"d-flex flex-row-reverse"},this.getTriggerNodeForClauses(e)),this.getTriggerNodeForNoWrapClause=()=>this.isSingleFilterAndMultipleClauses()&&!this.props.wrap&&(0,s.jsx)("div",{className:"ml-3"},this.getTriggerNodeForClauses()),this.endUserClausesNum=(0,r.getShownClauseNumberByExpression)(this.props.config.sqlExprObj),this.clausesNumConfigured=(0,r.getTotalClauseNumberByExpression)(this.props.config.sqlExprObj),this.state={isOpen:!1,applied:this.getAppliedState(),collapsed:this.props.config.collapseFilterExprs,sqlExprObj:this.props.config.sqlExprObj,sqlChanged:!1,outputWidgetLabel:this.getOutPutWidgetLabel(),popperVersion:1}}componentDidUpdate(e,t){this.endUserClausesNum=(0,r.getShownClauseNumberByExpression)(this.props.config.sqlExprObj),this.clausesNumConfigured=(0,r.getTotalClauseNumberByExpression)(this.props.config.sqlExprObj),e.config!==this.props.config||e.selectedDs!==this.props.selectedDs?this.setState({applied:this.getAppliedState(),collapsed:e.config.collapseFilterExprs!==this.props.config.collapseFilterExprs?this.props.config.collapseFilterExprs:this.state.collapsed,sqlExprObj:this.props.selectedDs?this.props.config.sqlExprObj:null,outputWidgetLabel:this.props.useDataSource.dataSourceId===e.useDataSource.dataSourceId?this.state.outputWidgetLabel:this.getOutPutWidgetLabel()}):e.logicalOperator===this.props.logicalOperator&&e.omitInternalStyle===this.props.omitInternalStyle||this.setState({applied:this.getAppliedState()})}isSingleFilterAndMultipleClauses(){return 1===this.props.filterNum&&this.clausesNumConfigured>1&&this.endUserClausesNum>=1}isSingleFilterAndSingleShownClause(){return 1===this.props.filterNum&&1===this.clausesNumConfigured&&1===this.endUserClausesNum}isMultipleFiltersAndSingleShownClause(){return this.props.filterNum>1&&1===this.clausesNumConfigured&&1===this.endUserClausesNum}render(){const{name:r,icon:a}=this.props.config;return(0,s.jsx)("div",{className:"filter-item",role:"group","aria-label":r},(0,s.jsx)(i.Card,{className:"filter-item-inline"},this.props.arrangeType===e.Block?(0,s.jsx)("div",{className:"w-100"},this.props.omitInternalStyle&&(this.isSingleFilterAndSingleShownClause()||this.isMultipleFiltersAndSingleShownClause())?(0,s.jsx)("div",{className:"w-100 pl-5 pr-5"},this.getSqlExpression()):(0,s.jsx)("div",{className:"filter-expanded-container"},this.getFilterItem(this.endUserClausesNum>=1))):(0,s.jsx)(s.React.Fragment,null,this.isSingleFilterAndSingleShownClause()?(0,s.jsx)("div",{className:"sql-expression-inline d-flex"},this.getSqlExpression(),!this.props.omitInternalStyle&&(0,s.jsx)("div",{className:"ml-3"},this.getTriggerNodeForClauses())):(0,s.jsx)(s.React.Fragment,null,this.isSingleFilterAndMultipleClauses()||this.isMultipleFiltersAndSingleShownClause()&&this.props.omitInternalStyle?(0,s.jsx)("div",{className:(0,s.classNames)("sql-expression-inline d-flex",{"sql-expression-wrap":this.props.wrap,"filter-item-pill":this.isMultipleFiltersAndSingleShownClause()})},this.getTirggerNodeForWrapClauses(t.Toggle),this.getSqlExpression(),this.getTirggerNodeForWrapClauses(t.Button),this.getTriggerNodeForNoWrapClause()):(0,s.jsx)("div",{className:"filter-popper-container"},this.props.triggerType===t.Toggle&&0===this.endUserClausesNum?(0,s.jsx)(i.Card,{className:"filter-item-pill filter-item-toggle-pill"},a&&(0,s.jsx)(i.Icon,{icon:a.svg,size:a.properties.size,className:"mr-1"}),(0,s.jsx)("div",{className:"filter-item-name toggle-name"},r),this.getToggle()):(0,s.jsx)("div",{className:"filter-item-pill h-100 nowrap"},(0,s.jsx)(i.Button,{className:(0,s.classNames)("",{"frame-active":this.state.applied}),title:r,ref:e=>{this.pillButton=e},type:"default","aria-pressed":this.state.applied,onClick:e=>this.onPillClick(this.endUserClausesNum>=1,this.pillButton)},a&&(0,s.jsx)(i.Icon,{icon:a.svg,size:a.properties.size}),r)),this.endUserClausesNum>=1&&(0,s.jsx)(i.Popper,{open:this.state.isOpen,toggle:this.onTogglePopper,modifiers:c,showArrow:!0,reference:this.pillButton,version:this.state.popperVersion,autoFocus:this.state.popperVersion>1,forceLatestFocusElements:this.props.triggerType===t.Button},(0,s.jsx)("div",{className:"filter-items-container",css:l(this.props.theme)},(0,s.jsx)("div",{className:"filter-item filter-item-popper overflow-auto",style:{maxHeight:"calc(100vh - 20px)"}},(0,s.jsx)(i.Card,{className:"filter-item-inline"},this.getFilterItem(this.endUserClausesNum>=1,this.props.arrangeType!==e.Popper))))))))))}}var h=function(e,t,s,i){return new(s||(s=Promise))((function(r,a){function o(e){try{n(i.next(e))}catch(e){a(e)}}function l(e){try{n(i.throw(e))}catch(e){a(e)}}function n(e){var t;e.done?r(e.value):(t=e.value,t instanceof s?t:new s((function(e){e(t)}))).then(o,l)}n((i=i.apply(e,t||[])).next())}))};class g extends s.BaseVersionManager{constructor(){super(...arguments),this.versions=[{version:"1.1.0",description:"",upgrader:i=>h(this,void 0,void 0,(function*(){return yield function(e){return h(this,void 0,void 0,(function*(){const t=[],i=s.DataSourceManager.getInstance();return e&&e.forEach((e=>{e.sqlExprObj&&t.push(i.createDataSourceByUseDataSource(Object.assign({},e.dataSource,{mainDataSourceId:e.dataSource.dataSourceId})))})),Promise.all(t)}))}(i.filterItems).then((s=>{let a=i;a=a.set("arrangeType",e.Block),a=a.set("triggerType",t.Toggle),a=a.set("wrap",!1),a=a.set("omitInternalStyle",!1);const o=s.map(((e,t)=>{const s=a.filterItems[t];return Object.assign({},s,{sqlExprObj:s.sqlExprObj?(0,r.updateSQLExpressionByVersion)(s.sqlExprObj,"1.1.0",e):null,icon:s.icon.setIn(["properties","color"],null),useDataSource:Object.assign({},s.dataSource,{mainDataSourceId:s.dataSource.dataSourceId})})}));return a=a.set("filterItems",o),a}))}))}]}}const m=new g;class f extends s.React.PureComponent{constructor(){super(...arguments),this.onDataSourceCreated=e=>{this.props.onCreateDataSourceCreatedOrFailed(this.props.useDataSource.dataSourceId,e)},this.onDataSourceInfoChange=e=>{this.props.onIsDataSourceNotReady(this.props.useDataSource.dataSourceId,null==e?void 0:e.status)},this.onCreateDataSourceFailed=()=>{this.props.onCreateDataSourceCreatedOrFailed(this.props.useDataSource.dataSourceId,null)}}componentWillUnmount(){this.props.onCreateDataSourceCreatedOrFailed(this.props.useDataSource.dataSourceId,null),this.props.onIsDataSourceNotReady(this.props.useDataSource.dataSourceId,s.DataSourceStatus.NotReady)}render(){const{useDataSource:e}=this.props;return(0,s.jsx)(s.DataSourceComponent,{useDataSource:e,onDataSourceCreated:this.onDataSourceCreated,onCreateDataSourceFailed:this.onCreateDataSourceFailed,onDataSourceInfoChange:this.onDataSourceInfoChange})}}var S=a(27218),x=a.n(S);const y=e=>{const t=window.SVG,{className:i}=e,r=function(e,t){var s={};for(var i in e)Object.prototype.hasOwnProperty.call(e,i)&&t.indexOf(i)<0&&(s[i]=e[i]);if(null!=e&&"function"==typeof Object.getOwnPropertySymbols){var r=0;for(i=Object.getOwnPropertySymbols(e);r<i.length;r++)t.indexOf(i[r])<0&&Object.prototype.propertyIsEnumerable.call(e,i[r])&&(s[i[r]]=e[i[r]])}return s}(e,["className"]),a=(0,s.classNames)("jimu-icon jimu-icon-component",i);return t?s.React.createElement(t,Object.assign({className:a,src:x()},r)):s.React.createElement("svg",Object.assign({className:a},r))},w=a(33679),b=[(0,i.getCustomFlipModifier)({fallbackPlacements:["top","left","right"],useClosestVerticalPlacement:!0}),i.maxSizeModifier,i.applyMaxSizeModifier];class v extends s.React.PureComponent{constructor(a){super(a),this.setSqlToAllDs=()=>{Object.keys(this.state.dataSources).forEach((e=>{const t=this.state.dataSources[e];if(t){const e=this.getQuerySqlFromDs(t);this.setSqlToDs(t,e)}}))},this.onFilterItemChange=(e,t,i,r)=>{if(this.__unmount)return;if(!this.state.filterItems[e]&&r)return void this.setSqlToDs(t,"");const a=this.state.filterItems.asMutable({deep:!0}),o=!(!r&&!a[e].autoApplyWhenWidgetOpen),l=Object.assign({},a[e],{sqlExprObj:i,autoApplyWhenWidgetOpen:r});a.splice(e,1,l);const n=(0,s.Immutable)(a);if(this.setState({filterItems:n}),o){const e=this.getQuerySqlFromDs(t,n);this.setSqlToDs(t,e)}},this.setSqlToDs=(e,t)=>{var i,r;if(this._autoApplyInit&&""===t)return;const a={where:t};e&&(null===(r=(i=e).updateQueryParams)||void 0===r||r.call(i,a,this.props.id),s.MessageManager.getInstance().publishMessage(new s.DataSourceFilterChangeMessage(this.props.id,[e.id])))},this.getQuerySqlFromDs=(e,t=this.props.config.filterItems)=>{const i=[];t.forEach((t=>{if(t.useDataSource.dataSourceId===(null==e?void 0:e.id)&&(t.autoApplyWhenWidgetOpen||this.props.config.omitInternalStyle&&1===(0,r.getShownClauseNumberByExpression)(t.sqlExprObj))){const r=s.dataSourceUtils.getArcGISSQL(t.sqlExprObj,e).sql;r&&i.push(r)}}));let a="";return i.length&&(a=1===i.length?i[0]:"("+i.join(") "+this.props.config.logicalOperator+" (")+")"),a},this.getDataSourceById=e=>{const t=this.props.useDataSources.asMutable({deep:!0}).filter((t=>t.dataSourceId===e));return(0,s.Immutable)(t[0])},this.isDataSourceRemoved=e=>{var t;return 0===(null===(t=this.props.useDataSources)||void 0===t?void 0:t.filter((t=>e===t.dataSourceId)).length)},this.onResetChange=()=>{this.setState({filterItems:this.props.config.filterItems}),this.setSqlToAllDs()},this.showResetAtBottom=(t,s,i,a)=>{let o=!0;return t&&s===e.Inline&&!i&&(a.length>1||1===a.length&&0===(0,r.getShownClauseNumberByExpression)(a[0].sqlExprObj))&&(o=!1),o},this.getFilterItems=(r,a=e.Block,o=!1,n=!1)=>{const p=this.showResetAtBottom(r.resetAll,a,o,r.filterItems);return(0,s.jsx)("div",{className:(0,s.classNames)("w-100 h-100 d-flex justify-content-between",p?"flex-column":"flex-row"),css:l(this.props.theme)},(0,s.jsx)("div",{className:(0,s.classNames)("w-100 filter-items-container",a&&r.arrangeType===e.Inline?"filter-items-inline":"",o?"filter-items-wrap":"",n?"filter-items-popup":"")},this.state.filterItems.map(((e,t)=>{const i=this.isDataSourceRemoved(e.useDataSource.dataSourceId)?null:this.state.dataSources[e.useDataSource.dataSourceId],l=this.state.outputDataSourceIsNotReady[e.useDataSource.dataSourceId];return(0,s.jsx)(d,{key:t,id:t,widgetId:this.props.id,intl:this.props.intl,selectedDs:i,useDataSource:e.useDataSource,isNotReadyFromWidget:l,logicalOperator:r.logicalOperator,config:e,arrangeType:a,triggerType:r.triggerType,wrap:o,omitInternalStyle:r.omitInternalStyle,filterNum:this.state.filterItems.length,onChange:this.onFilterItemChange,itemBgColor:this.props.theme.colors.palette.light[300],theme:this.props.theme})}))),r.resetAll&&(0,s.jsx)("div",{className:(0,s.classNames)("filter-reset-container",p?"bottom-reset":"right-reset")},(0,s.jsx)(i.Button,{icon:!0,type:"default",size:"default",className:"reset-button",style:{borderRadius:r.triggerType===t.Toggle?"16px":null},title:this.props.intl.formatMessage({id:"resetAllFilters",defaultMessage:i.defaultMessages.resetAllFilters}),"aria-label":this.props.intl.formatMessage({id:"resetAllFilters",defaultMessage:i.defaultMessages.resetAllFilters}),onClick:this.onResetChange},(0,s.jsx)(y,null))))},this.onShowPopper=()=>{this.setState({isOpen:!this.state.isOpen,popperVersion:this.state.isOpen?this.state.popperVersion:this.state.popperVersion+1})},this.onTogglePopper=()=>{this.setState({isOpen:!1}),s.lodash.defer((()=>{this.widgetIconRef.focus()}))},this.checkIfAnyFiltersApplied=()=>{var e;const{omitInternalStyle:t}=this.props.config;return((null===(e=this.state)||void 0===e?void 0:e.filterItems)||this.props.config.filterItems).some((e=>t&&1===(0,r.getTotalClauseNumberByExpression)(e.sqlExprObj)&&1===(0,r.getShownClauseNumberByExpression)(e.sqlExprObj)?""!==(this.state.dataSources[e.useDataSource.dataSourceId]?s.dataSourceUtils.getArcGISSQL(e.sqlExprObj,this.state.dataSources[e.useDataSource.dataSourceId]).sql:e.sqlExprObj.sql):e.autoApplyWhenWidgetOpen))},this.onIsDataSourceNotReady=(e,t)=>{this.setState((i=>{var r;if(!(null===(r=i.dataSources[e])||void 0===r?void 0:r.getDataSourceJson().isOutputFromWidget))return;const a=Object.assign({},i.outputDataSourceIsNotReady);return a[e]=t===s.DataSourceStatus.NotReady,{outputDataSourceIsNotReady:a}}))},this.onCreateDataSourceCreatedOrFailed=(e,t)=>{this.setState((s=>{const i=Object.assign({},s.dataSources);return i[e]=t,{dataSources:i}}))},this.applyAutoFiltersAtStart=()=>{var e;this._autoApplyInit&&Object.keys(this.state.dataSources).map((()=>!0)).length===(null===(e=this.props.useDataSources)||void 0===e?void 0:e.length)&&setTimeout((()=>{this.setSqlToAllDs(),this._autoApplyInit=!1}),0)},this.__unmount=!1,this.index=0,this._autoApplyInit=!0,this.state={popperVersion:1,isOpen:!1,filterItems:this.props.config.filterItems,dataSources:{},outputDataSourceIsNotReady:{}}}componentWillUnmount(){this.__unmount=!0,Object.keys(this.state.dataSources).forEach((e=>{const t=this.state.dataSources[e];t&&(null==t||t.updateQueryParams(null,this.props.id),s.MessageManager.getInstance().publishMessage(new s.DataSourceFilterChangeMessage(this.props.id,[t.id])))}))}componentDidUpdate(e,t){this.__unmount||(this._autoApplyInit=!1,e.config!==this.props.config?(this.setState({filterItems:this.props.config.filterItems}),this.setSqlToAllDs()):this.state.dataSources!==t.dataSources&&(this._autoApplyInit=!0,this.applyAutoFiltersAtStart()))}render(){var t;const{config:r,icon:a,label:o}=this.props;return 0===this.state.filterItems.length?(0,s.jsx)(i.WidgetPlaceholder,{icon:w,widgetId:this.props.id,message:this.props.intl.formatMessage({id:"_widgetLabel",defaultMessage:"Filter"})}):(0,s.jsx)("div",{className:"jimu-widget widget-filter overflow-auto"},null===(t=this.props.useDataSources)||void 0===t?void 0:t.map(((e,t)=>(0,s.jsx)(f,{key:t,useDataSource:e,onIsDataSourceNotReady:this.onIsDataSourceNotReady,onCreateDataSourceCreatedOrFailed:this.onCreateDataSourceCreatedOrFailed}))),r.arrangeType===e.Popper?(0,s.jsx)("div",{className:"filter-widget-popper"},(0,s.jsx)(i.Badge,{dot:!0,className:"m-1",hideBadge:!this.checkIfAnyFiltersApplied(),color:"primary"},(0,s.jsx)(i.Button,{icon:!0,size:"sm",className:"filter-widget-pill h-100",ref:e=>{this.widgetIconRef=e},title:o,type:"tertiary",onClick:this.onShowPopper,"aria-pressed":this.checkIfAnyFiltersApplied()},(0,s.jsx)(i.Icon,{icon:"string"==typeof a?a:a.svg,size:16,color:"string"==typeof a?"":a.properties.color}))),this.state.popperVersion>1&&(0,s.jsx)(i.Popper,{open:this.state.isOpen,version:this.state.popperVersion,keepMount:!0,toggle:this.onTogglePopper,showArrow:!0,modifiers:b,forceLatestFocusElements:!0,reference:this.widgetIconRef},(0,s.jsx)("div",{className:"p-2"},this.getFilterItems(r,e.Block,!1,!0)))):(0,s.jsx)("div",{className:"w-100 h-100"},this.getFilterItems(r,r.arrangeType,r.wrap)))}}v.versionManager=m})(),o})())}}}));