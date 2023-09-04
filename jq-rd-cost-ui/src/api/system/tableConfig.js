import request from '@/utils/jqRequest'
import excelDownLoad from '@/utils/exceldownload'

// 查询表格列配置列表
export function listTableConfig(query) {
  return request({
    url: '/system/tableConfig/list',
    method: 'get',
    params: query
  })
}

// 查询表格列配置详细
export function getTableConfig(id) {
  return request({
    url: '/system/tableConfig/' + id,
    method: 'get'
  })
}

// 新增表格列配置
export function addTableConfig(data) {
  return request({
    url: '/system/tableConfig',
    method: 'post',
    data: data
  })
}

// 修改表格列配置
export function updateTableConfig(data) {
  return request({
    url: '/system/tableConfig',
    method: 'put',
    data: data
  })
}

// 删除表格列配置
export function delTableConfig(id) {
  return request({
    url: '/system/tableConfig/' + id,
    method: 'delete'
  })
}

// 导出表格列配置
export function exportTableConfig(query) {
  return excelDownLoad('/system/tableConfig/export', query)
}

// 是否固定表格列
export function lockColumn(data) {
  return request({
    url: '/system/tableConfig/lockColumn',
    method: 'post',
    data: data
  })
}

// 是否显示列
export function showColumn(data) {
  return request({
    url: '/system/tableConfig/showColumn',
    method: 'post',
    data: data
  })
}

// 获取表格
export function getTableDate(url,method,params) {
  return request({
    url,
    method,
    params
  })
}

// 导出选中行
export function exportCheckRow(url,params,e) {
  return excelDownLoad(url,params,e,'jq_table')
}
