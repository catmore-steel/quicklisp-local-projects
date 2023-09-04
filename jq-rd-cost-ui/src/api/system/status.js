import request from '@/utils/jqRequest'
import excelDownLoad from '@/utils/exceldownload'

export function listFileStatus(query) {
  return request({
    url: '/system/status/listFileStatus',
    method: 'get',
    params: query
  })
}

export function saveFileStatus(data) {
  return request({
    url: '/system/status/saveFileStatus',
    method: 'post',
    data: data
  })
}

// 查询材料状态配置详细
export function getStatus(id) {
  return request({
    url: '/system/status/' + id,
    method: 'get'
  })
}

// 新增材料状态配置
export function addStatus(data) {
  return request({
    url: '/system/status',
    method: 'post',
    data: data
  })
}

// 修改材料状态配置
export function updateStatus(data) {
  return request({
    url: '/system/status',
    method: 'put',
    data: data
  })
}

// 删除材料状态配置
export function delStatus(id) {
  return request({
    url: '/system/status/' + id,
    method: 'delete'
  })
}

// 导出材料状态配置
export function exportStatus(query) {
  return excelDownLoad('/system/status/export',query)
}
