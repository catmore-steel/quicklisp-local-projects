import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询无形资产信息详细
export function getIpInfo(id) {
  return request({
    url: '/cost/ipInfo/getIpById',
    method: 'get',
    params: { id: id }
  })
}

// 新增无形资产信息
export function addIpInfo(data) {
  return request({
    url: '/cost/ipInfo/save',
    method: 'post',
    data: data
  })
}

// 修改无形资产信息
export function updateIpInfo(data) {
  return request({
    url: '/cost/ipInfo',
    method: 'put',
    data: data
  })
}

// 删除无形资产信息
export function delIpInfo(id) {
  return request({
    url: '/cost/ipInfo/' + id,
    method: 'delete'
  })
}

// 导出无形资产信息
export function exportIpInfo(query) {
  return excelDownLoad('/cost/ipInfo/export', query)
}
