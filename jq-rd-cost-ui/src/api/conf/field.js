import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

export function listField(query) {
  return request({
    url: '/conf/technicalField/listField',
    method: 'get',
    params: query
  })
}

// 查询技术领域详细
export function getField(id) {
  return request({
    url: '/conf/technicalField/' + id,
    method: 'get'
  })
}

// 新增技术领域
export function addField(data) {
  return request({
    url: '/conf/technicalField',
    method: 'post',
    data: data
  })
}

// 修改技术领域
export function updateField(data) {
  return request({
    url: '/conf/technicalField',
    method: 'put',
    data: data
  })
}

// 删除技术领域
export function delField(id) {
  return request({
    url: '/conf/technicalField/' + id,
    method: 'delete'
  })
}

// 导出技术领域
export function exportField(query) {
  return excelDownLoad('/conf/technicalField/export',query)
}
