import request from '@/utils/jqRequest'
import excelDownLoad from '@/utils/exceldownload'



// 查询系统邮件管理列表
export function listMailinfo(query) {
  return request({
    url: '/system/mailinfo/list',
    method: 'get',
    params: query
  })
}

// 查询系统邮件管理详细
export function getMailinfo(id) {
  return request({
    url: '/system/mailinfo/' + id,
    method: 'get'
  })
}

// 新增系统邮件管理
export function addMailinfo(data) {
  return request({
    url: '/system/mailinfo',
    method: 'post',
    data: data
  })
}

// 修改系统邮件管理
export function updateMailinfo(data) {
  return request({
    url: '/system/mailinfo',
    method: 'put',
    data: data
  })
}

// 删除系统邮件管理
export function delMailinfo(id) {
  return request({
    url: '/system/mailinfo/' + id,
    method: 'delete'
  })
}

// 导出系统邮件管理
export function exportMailinfo(query) {
  return excelDownLoad('/system/mailinfo/export',query)
}

//重新发送邮件
export function btnReSend(ids) {
  return request({
    url: '/system/mailinfo/btnReSend/'+ids,
    method: 'post',
  })
}
