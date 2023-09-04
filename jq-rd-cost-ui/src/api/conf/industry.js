import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

export function listIndustry(query) {
  return request({
    url: '/conf/industry/listIndustry',
    method: 'get',
    params: query
  })
}

export function treeselectIndustry(query) {
  return request({
    url: '/conf/industry/treeselect',
    method: 'get',
    params: query
  })
}

// 查询行业分类详细
export function getIndustry(id) {
  return request({
    url: '/conf/industry/' + id,
    method: 'get'
  })
}

// 新增行业分类
export function addIndustry(data) {
  return request({
    url: '/conf/industry',
    method: 'post',
    data: data
  })
}

// 修改行业分类
export function updateIndustry(data) {
  return request({
    url: '/conf/industry',
    method: 'put',
    data: data
  })
}

// 删除行业分类
export function delIndustry(id) {
  return request({
    url: '/conf/industry/' + id,
    method: 'delete'
  })
}

// 导出行业分类
export function exportIndustry(query) {
  return excelDownLoad('/conf/industry/export',query)
}
