import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

export function listUserConfig(query) {
  return request({
    url: '/system/userConfig/listUserConfig',
    method: 'get',
    params: query
  })
}

export function getCheckingUserList() {
  return request({
    url: '/system/userConfig/listCheckingUser',
    method: 'get'
  })
}

// 查询用户配置详细
export function getConfig(id) {
  return request({
    url: '/system/userConfig/' + id,
    method: 'get'
  })
}

// 新增用户配置
export function addConfig(data) {
  return request({
    url: '/system/userConfig',
    method: 'post',
    data: data
  })
}

// 修改用户配置
export function updateConfig(data) {
  return request({
    url: '/system/userConfig',
    method: 'put',
    data: data
  })
}

// 删除用户配置
export function delConfig(id) {
  return request({
    url: '/system/userConfig/' + id,
    method: 'delete'
  })
}

// 导出用户配置
export function exportConfig(query) {
  return excelDownLoad('/system/userConfig/export',query)
}
