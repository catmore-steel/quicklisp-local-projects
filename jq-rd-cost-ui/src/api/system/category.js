import request from '@/utils/jqRequest'
import excelDownLoad from '@/utils/exceldownload'

// 查询流程分类列表
export function listCategory(query) {
  return request({
    url: '/system/category/list',
    method: 'get',
    params: query
  })
}

export function getCategorySelectList() {
  return request({
    url: '/system/category/getCategorySelectList',
    method: 'get',
  })
}

// 查询流程分类详细
export function getCategory(id) {
  return request({
    url: '/system/category/' + id,
    method: 'get'
  })
}

// 新增流程分类
export function addCategory(data) {
  return request({
    url: '/system/category',
    method: 'post',
    data: data
  })
}

// 修改流程分类
export function updateCategory(data) {
  return request({
    url: '/system/category',
    method: 'put',
    data: data
  })
}

// 删除流程分类
export function delCategory(id) {
  return request({
    url: '/system/category/' + id,
    method: 'delete'
  })
}

// 导出流程分类
export function exportCategory(query) {
  return excelDownLoad('/system/category/export',query)
}
