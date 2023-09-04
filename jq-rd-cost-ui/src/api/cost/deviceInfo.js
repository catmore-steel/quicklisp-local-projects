import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询企业设备信息详细
export function getDeviceInfo(deviceInfoId) {
  return request({
    url: '/cost/deviceInfo/getDeviceById',
    method: 'get',
    params: { deviceInfoId: deviceInfoId }
  })
}

// 新增企业设备信息
export function addDeviceInfo(data) {
  return request({
    url: '/cost/deviceInfo/save',
    method: 'post',
    data: data
  })
}

// 修改企业设备信息
export function updateDeviceInfo(data) {
  return request({
    url: '/cost/deviceInfo/update',
    method: 'put',
    data: data
  })
}

// 删除企业设备信息
export function delDeviceInfo(id) {
  return request({
    url: '/cost/deviceInfo/delete/' + id,
    method: 'delete'
  })
}

// 导出企业设备信息
export function exportDeviceInfo(query) {
  return excelDownLoad('/cost/deviceInfo/export', query)
}
