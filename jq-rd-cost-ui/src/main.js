import Vue from 'vue'

import Cookies from 'js-cookie'

import 'normalize.css/normalize.css' // a modern alternative to CSS resets

import Element from 'element-ui'
import 'default-passive-events'
import './assets/styles/element-variables.scss'

import '@/assets/iconfont/iconfont.css' // iconfont css

import '@/assets/styles/form.scss'
import '@/assets/styles/index.scss' // global css
import '@/assets/styles/jq.scss' // jq css
import App from './App'
import store from './store'
import router from './router'
import permission from './directive/permission'
import Print from 'vue-print-nb'

import './assets/icons' // icon
import './permission' // permission control
import {getDicts, getFlowDicts} from "./api/system/dict/data";
import {getConfigKey} from "./api/system/config";
import {
  parseTime,
  resetForm,
  addDateRange,
  selectDictLabel,
  selectDictLabels,
  download,
  handleTree,
  choseAttachByRelateKey,
  sortByColumn,
  isNullOrEmpty,
  choseOption,
  isFlowStatus
} from "./utils/jq";
import Pagination from "./components/Pagination";
import RightToolbar from "./components/RightToolbar"   //自定义表格工具扩展
import ExportDialog from './components/ExportDialog' //导出工具
import ImportDialog from './components/ImportDialog' //导入工具
import ImportZipDialog from './components/ImportZipDialog' //导入工具
import JqReadPdf from './components/JqReadPdf'  //PDF工具
import JqAttach from './components/JqAttach' //附件工具
import JqUserSelect from './components/JqUserSelect'  //JqUserSelect 提交案人选择器
import JqAgentUserSelect from './components/JqAgentUserSelect'  //提交代理人选择器
import JqPanel from './components/JqPanel'  //JQ折叠面板
import JqDialog from './components/JqDialog' //JQ弹出框
import JqCard from './components/JqCard' //JQ侧边弹出框
import JqTable from './components/JqTable' //JQ数据表格
import JqSearch from './components/JqSearch'  //JQ顶部查询组件
import JqCheckUser from './components/JqCheckUser'  //JQ校验当前登录人
import JqDictSelect from './components/JqDictSelect'  //JQ 字典下拉
import Treeselect from "@riophae/vue-treeselect";
import echarts from 'echarts'

import JqSelectCompany from './components/JqSelectCompany'  //高企撰写全局客户ID
import JqSelectItem from './components/JqSelectItem'  //高企撰写全局项目No


import "@riophae/vue-treeselect/dist/vue-treeselect.css";

const defaultSettings = require('../src/settings.js')
Vue.use(Print)

// 全局方法挂载
Vue.prototype.getDicts = getDicts
Vue.prototype.getFlowDicts = getFlowDicts
Vue.prototype.getConfigKey = getConfigKey
Vue.prototype.parseTime = parseTime
Vue.prototype.resetForm = resetForm
Vue.prototype.addDateRange = addDateRange
Vue.prototype.selectDictLabel = selectDictLabel
Vue.prototype.selectDictLabels = selectDictLabels
Vue.prototype.download = download
Vue.prototype.handleTree = handleTree
Vue.prototype.choseAttachByRelateKey = choseAttachByRelateKey
Vue.prototype.sortByColumn = sortByColumn
Vue.prototype.isNullOrEmpty = isNullOrEmpty
Vue.prototype.choseOption = choseOption
Vue.prototype.isFlowStatus = isFlowStatus
Vue.prototype.EventBus = new Vue()
Vue.prototype.$bus = new Vue()
Vue.prototype.$echarts = echarts;

Vue.prototype.msgSuccess = function (msg) {
  this.$message({showClose: true, message: msg, type: "success"});
}

Vue.prototype.msgError = function (msg) {
  this.$message({showClose: true, message: msg, type: "error"});
}

Vue.prototype.msgInfo = function (msg) {
  this.$message.info(msg);
}



// 全局组件挂载
Vue.component('Pagination', Pagination)
Vue.component('RightToolbar', RightToolbar)
Vue.component('ExportDialog', ExportDialog)
Vue.component('JqAttach', JqAttach)
Vue.component('ImportDialog', ImportDialog)
Vue.component('ImportZipDialog', ImportZipDialog)
Vue.component('JqReadPdf', JqReadPdf)
Vue.component('JqUserSelect', JqUserSelect)
Vue.component('JqAgentUserSelect', JqAgentUserSelect)
Vue.component('JqPanel', JqPanel)
Vue.component('JqDialog', JqDialog)
Vue.component('JqCard', JqCard)
Vue.component('JqTable', JqTable)
Vue.component('Treeselect', Treeselect)
Vue.component('JqSearch', JqSearch)
Vue.component('JqCheckUser', JqCheckUser)

Vue.component('JqSelectCompany', JqSelectCompany)
Vue.component('JqSelectItem', JqSelectItem)
Vue.component('JqDictSelect', JqDictSelect)
Vue.use(permission)

/**
 * If you don't want to use mock-server
 * you want to use MockJs for mock api
 * you can execute: mockXHR()
 *
 * Currently MockJs will be used in the production environment,
 * please remove it before going online! ! !
 */

//全局修改弹窗黑幕点击关闭弹窗默认组件的配置
//Dialog
Element.Dialog.props.closeOnClickModal.default = false;
//Table
Element.Table.props.border = {type: Boolean, default: true};
Element.Table.props.stripe = {type: Boolean, default: true};
//表头背景色
Element.Table.props.headerCellStyle = {
  type: [Function], default: function () {
    return [{background: '#eef1f6', color: '#606266'}];
  }
};
//超出部分隐藏
Element.TableColumn.props.showOverflowTooltip = { type: Boolean, default: true };
//默认开启所有tableColumn后端排序
// Element.TableColumn.props.sortable = { type: [Boolean, String], default: 'custom' };
Element.TableColumn.props.align = { type: String, default: 'center' };
//设置select默认支持筛选并取消
Element.Select.props.clearable = { type: Boolean, default: true };
Element.Select.props.filterable = { type: Boolean, default: true };


// Element.Input.props.autosize = {type: Boolean, default: true};
// set element-ui default size
Vue.use(Element, { size: Cookies.get('size') || defaultSettings.size  })
Vue.config.productionTip = false

new Vue({
  el: '#app', router, store, render: h => h(App)
})
