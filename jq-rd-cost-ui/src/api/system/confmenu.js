import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'


// 查询申报机构列表
export function treeList(query) {
  return request({
    url: '/system/confmenu/treeList',
    method: 'get',
    params: query
  })
}

// 新增整本材料目录配置
export function saveSubmit(data) {
  return request({
    url: '/system/confmenu/saveSubmit',
    method: 'post',
    data: data
  })
}


// 查询整本材料目录配置详细
export function getConfmenu(id) {
  return request({
    url: '/system/confmenu/' + id,
    method: 'get'
  })
}

// 新增整本材料目录配置
export function addConfmenu(data) {
  return request({
    url: '/system/confmenu',
    method: 'post',
    data: data
  })
}

// 修改整本材料目录配置
export function updateConfmenu(data) {
  return request({
    url: '/system/confmenu',
    method: 'put',
    data: data
  })
}

// 删除整本材料目录配置
export function delConfmenu(id) {
  return request({
    url: '/system/confmenu/' + id,
    method: 'delete'
  })
}

// 导出整本材料目录配置
export function exportConfmenu(query) {
  return excelDownLoad('/system/confmenu/export',query)
}
