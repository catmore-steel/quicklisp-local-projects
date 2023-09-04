import hasRole from './hasRole'
import hasPermi from './hasPermi'
import copy from './copyText'
const install = function(Vue) {
  Vue.directive('hasRole', hasRole)
  Vue.directive('hasPermi', hasPermi)
  Vue.directive('copy', copy)
}

if (window.Vue) {
  window['hasRole'] = hasRole
  window['hasPermi'] = hasPermi
  window['copy'] = copy
  Vue.use(install); // eslint-disable-line
}

export default install
