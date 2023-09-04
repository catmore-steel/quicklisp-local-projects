import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询研发投入设备折旧费用拟定;详细
export function getFee(id) {
  return request({
    url: '/cost/budgetDeviceFee/' + id,
    method: 'get'
  })
}

// 新增研发投入设备折旧费用拟定;
export function addFee(data) {
  return request({
    url: '/cost/budgetDeviceFee',
    method: 'post',
    data: data
  })
}

// 修改研发投入设备折旧费用拟定;
export function updateFee(data) {
  return request({
    url: '/cost/budgetDeviceFee',
    method: 'put',
    data: data
  })
}

// 删除研发投入设备折旧费用拟定;
export function delFee(id) {
  return request({
    url: '/cost/budgetDeviceFee/' + id,
    method: 'delete'
  })
}

// 导出研发投入设备折旧费用拟定;
export function exportFee(query) {
  return excelDownLoad('/cost/budgetDeviceFee/export',query)
}

export function getBaseDeviceInfo(id) {
  return request({
    url: '/cost/budgetDeviceFee/deviceInfo/' + id,
    method: 'get'
  })
}

export function updateByDeviceInfo(data) {
  return request({
    url: '/cost/budgetDeviceFee/updateByDeviceInfo',
    method: 'post',
    data: data
  })
}

export function getStatistics(query) {
  console.log('getStatistics',query)
  return request({
    url: '/cost/budgetDeviceFee/statistics',
    method: 'get',
    params: query
  })
}

export function delByDeviceInfoId(data) {
  return request({
    url: '/cost/budgetDeviceFee/delByDeviceInfoId/'+data,
    method: 'post',
    data: data
  })
}
