import request from '@/utils/jqRequest'
import excelDownLoad from '@/utils/exceldownload'



// 查询节假日信息列表
export function listHolidayInfo(query) {
  return request({
    url: '/system/holidayInfo/list',
    method: 'get',
    params: query
  })
}

// 查询节假日信息详细
export function getHolidayInfo(id) {
  return request({
    url: '/system/holidayInfo/' + id,
    method: 'get'
  })
}

// 新增节假日信息
export function addHolidayInfo(data) {
  return request({
    url: '/system/holidayInfo',
    method: 'post',
    data: data
  })
}

// 修改节假日信息
export function updateHolidayInfo(data) {
  return request({
    url: '/system/holidayInfo',
    method: 'put',
    data: data
  })
}

// 删除节假日信息
export function delHolidayInfo(id) {
  return request({
    url: '/system/holidayInfo/' + id,
    method: 'delete'
  })
}

// 导出节假日信息
export function exportHolidayInfo(query) {
  return excelDownLoad('/system/holidayInfo/export',query)
}
