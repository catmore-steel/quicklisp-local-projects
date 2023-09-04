import request from '@/utils/jqRequest'
import excelDownLoad from '@/utils/exceldownload'



// 查询系统消息列表
export function listMessageinfo(query) {
  return request({
    url: '/system/messageinfo/list',
    method: 'get',
    params: query
  })
}

// 查询系统消息详细
export function getMessageinfo(id) {
  return request({
    url: '/system/messageinfo/' + id,
    method: 'get'
  })
}

// 新增系统消息
export function addMessageinfo(data) {
  return request({
    url: '/system/messageinfo',
    method: 'post',
    data: data
  })
}

// 修改系统消息
export function updateMessageinfo(data) {
  return request({
    url: '/system/messageinfo',
    method: 'put',
    data: data
  })
}

// 删除系统消息
export function delMessageinfo(id) {
  return request({
    url: '/system/messageinfo/' + id,
    method: 'delete'
  })
}

// 导出系统消息
export function exportMessageinfo(query) {
  return excelDownLoad('/system/messageinfo/export',query)
}
