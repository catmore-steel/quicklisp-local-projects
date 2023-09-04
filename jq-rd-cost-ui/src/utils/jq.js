/**
 * 通用js方法封装处理
 * Copyright (c) 2019 jq
 */
// const defaultSettings = require('./src/settings.js')
const baseURL = process.env.VUE_APP_BASE_API
const defaultSettings = require('../settings')


// 日期格式化
export function parseTime(time, pattern) {
  if (arguments.length === 0 || !time) {
    return null
  }
  const format = pattern || '{y}-{m}-{d} {h}:{i}:{s}'
  let date
  if (typeof time === 'object') {
    date = time
  } else {
    if ((typeof time === 'string') && (/^[0-9]+$/.test(time))) {
      time = parseInt(time)
    } else if (typeof time === 'string') {
      time = time.replace(new RegExp(/-/gm), '/');
    }
    if ((typeof time === 'number') && (time.toString().length === 10)) {
      time = time * 1000
    }
    date = new Date(time)
  }
  const formatObj = {
    y: date.getFullYear(),
    m: date.getMonth() + 1,
    d: date.getDate(),
    h: date.getHours(),
    i: date.getMinutes(),
    s: date.getSeconds(),
    a: date.getDay()
  }
  const time_str = format.replace(/{(y|m|d|h|i|s|a)+}/g, (result, key) => {
    let value = formatObj[key]
    // Note: getDay() returns 0 on Sunday
    if (key === 'a') {
      return ['日', '一', '二', '三', '四', '五', '六'][value]
    }
    if (result.length > 0 && value < 10) {
      value = '0' + value
    }
    return value || 0
  })
  return time_str
}

// 表单重置
export function resetForm(refName) {
  if (this.$refs[refName]) {
    this.$refs[refName].resetFields();
  }
}

// 添加日期范围
export function addDateRange(params, dateRange) {
  var search = params;
  search.beginTime = "";
  search.endTime = "";
  if (null != dateRange && '' != dateRange) {
    search.beginTime = dateRange[0];
    search.endTime = dateRange[1];
  }
  return search;
}

// 回显数据字典
export function selectDictLabel(datas, value) {
  var actions = [];
  Object.keys(datas).some((key) => {
    if (datas[key].dictValue == ('' + value)) {
      actions.push(datas[key].dictLabel);
      return true;
    }
  })
  return actions.join('');
}

// 回显数据字典（字符串数组）
export function selectDictLabels(datas, value, separator) {
  var actions = [];
  var currentSeparator = undefined === separator ? "," : separator;
  var temp = value.split(currentSeparator);
  Object.keys(value.split(currentSeparator)).some((val) => {
    Object.keys(datas).some((key) => {
      if (datas[key].dictValue == ('' + temp[val])) {
        actions.push(datas[key].dictLabel + currentSeparator);
      }
    })
  })
  return actions.join('').substring(0, actions.join('').length - 1);
}

// 通用下载方法
export function download(fileName) {
  window.location.href = baseURL + "/common/download?fileName=" + encodeURI(fileName) + "&delete=" + true;
}

// 字符串格式化(%s )
export function sprintf(str) {
  var args = arguments, flag = true, i = 1;
  str = str.replace(/%s/g, function () {
    var arg = args[i++];
    if (typeof arg === 'undefined') {
      flag = false;
      return '';
    }
    return arg;
  });
  return flag ? str : '';
}

// 转换字符串，undefined,null等转化为""
export function praseStrEmpty(str) {
  if (!str || str == "undefined" || str == "null") {
    return "";
  }
  return str;
}

/**
 * 构造树型结构数据
 * @param {*} data 数据源
 * @param {*} id id字段 默认 'id'
 * @param {*} parentId 父节点字段 默认 'parentId'
 * @param {*} children 孩子节点字段 默认 'children'
 * @param {*} rootId 根Id 默认 0
 */
export function handleTree(data, id, parentId, children, rootId) {
  id = id || 'id'
  parentId = parentId || 'parentId'
  children = children || 'children'
  rootId = rootId || Math.min.apply(Math, data.map(item => {
    return item[parentId]
  })) || 0
  //对源数据深度克隆
  const cloneData = JSON.parse(JSON.stringify(data))
  //循环所有项
  const treeData = cloneData.filter(father => {
    let branchArr = cloneData.filter(child => {
      //返回每一项的子级数组
      return father[id] == child[parentId]
    });
    branchArr.length > 0 ? father.children = branchArr : '';
    //返回第一层
    return father[parentId] == rootId;
  });
  return treeData != '' ? treeData : data;
}

/**
 *  根据附件的key筛选出集合
 * @author rainfly
 * @param datas
 */
export function choseAttachByRelateKey(datas, relateKey) {
  var actions = [];
  Object.keys(datas).some((key) => {
    if (datas[key].relateKey == relateKey) {
      actions.push(datas[key]);
    }
  })
  return actions
}


/**
 * el-table 公共排序规则
 * @author rainfly
 * @param column
 */
export function sortByColumn(column) {
  //确认prop排序
  var orders = "";
  if (column instanceof Array) {
    //默认排序
    Object.keys(column).some((key) => {
      if (this.isNullOrEmpty(orders)) {
        if (this.isNullOrEmpty(column[key].columnKey)) {
          orders = column[key].prop.replace(/([A-Z])/g, "_$1").toLowerCase() + ' ' + column[key].order.replace("ending", "");
        } else {
          orders = column[key].columnKey + ' ' + column[key].order.replace("ending", "");
        }
      } else {
        if (this.isNullOrEmpty(column[key].columnKey)) {
          orders = orders + ',' + column[key].prop.replace(/([A-Z])/g, "_$1").toLowerCase() + ' ' + column[key].order.replace("ending", "");
        } else {
          orders = orders + ',' + column[key].columnKey + ' ' + column[key].order.replace("ending", "");
        }
      }
    })
  } else {
    //表格排序
    if (this.isNullOrEmpty(column.columnKey)) {
      orders = column.prop.replace(/([A-Z])/g, "_$1").toLowerCase();
    } else {
      orders = column.columnKey;
    }
    if (!this.isNullOrEmpty(column.order)) {
      orders = orders + " " + column.order.replace("ending", "");
    } else {
      // 排序为空，默认为升序
      orders = orders + " " + "asc";
    }
  }
  return orders
}

/**
 *   判断是否为空 如果为空则返回true
 * @param value
 * @returns {boolean}
 */
export function isNullOrEmpty(value) {
  if (value === null || value === undefined || value === '') {
    return true
  } else {
    return false
  }
}

/**
 * 判断 arr 数组中是否存在非value的数据 如果存在则返回 true 不存在则返false
 * @param arr value
 * @returns {boolean}
 */
export function choseOption(arr, value) {
  let status = false;
  if (arr.length == 0) {
    status = true;
  }
  arr.forEach(item => {
    if (item != value) {
      status = true;
    }
  });
  return status;
}



/**
 *  判断是否包含在流程状态数组内
 */
export function isFlowStatus(flowStatus, currentStatus) {
  let result = false
  if (isNullOrEmpty(flowStatus) || isNullOrEmpty(currentStatus)) {
    return result
  }
  if (flowStatus.indexOf(currentStatus) != -1) {
    result = true
  } else {
    result = false
  }
  return result
}


/**
 * addTime为相加前的时间， days 为 需要相加的天数
 */
export function addDate(addTime, days) {
  const date = new Date(addTime)
  date.setDate(date.getDate() + days);
  const year = date.getFullYear();
  let month = date.getMonth() + 1;
  let day = date.getDate();
  const mm = "'" + month + "'";
  const dd = "'" + day + "'";
  if(mm.length == 3) {
    month = "0" + month;
  }
  if(dd.length == 3) {
    day = "0" + day;
  }
  const time = year + '-' + month + '-' + day
  return new Date(time);
}

//日期时间格式化成字符串，date:日期时间 ， format:格式化方式，例如yyyy-MM-dd
export function dateTypeFormat(date, format) {
  console.log(date)
  let ret
  const opt = {
    'Y+': date.getFullYear().toString(), // 年
    'M+': (date.getMonth() + 1).toString(), // 月
    'd+': date.getDate().toString(), // 日
    'H+': date.getHours().toString(), // 时
    'm+': date.getMinutes().toString(), // 分
    'S+': date.getSeconds().toString() // 秒
    // 有其他格式化字符需求可以继续添加，必须转化成字符串
  }
  for (const k in opt) {
    ret = new RegExp('(' + k + ')').exec(format)
    if (ret) {
      format = format.replace(ret[1], (ret[1].length === 1) ? (opt[k]) : (opt[k].padStart(ret[1].length, '0')))
    }
  }
  return format
}

//获取日期的第一天和最后一天
export function getFirstDayOfYear (date, type){
  let firstDay = new Date(date.toString())
  let lastDay = new Date(date.toString())
  if(type == 'year'){
    firstDay.setMonth(0);
    firstDay.setDate(1);
    lastDay.setMonth(11);
    lastDay.setDate(31);
  }
  return [firstDay, lastDay]
}

