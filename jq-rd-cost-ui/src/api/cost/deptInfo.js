import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询企业部门信息详细
export function getDeptInfo(id) {
  return request({
    url: '/cost/deptInfo/' + id,
    method: 'get'
  })
}

// 新增企业部门信息
export function addDeptInfo(data) {
  return request({
    url: '/cost/deptInfo',
    method: 'post',
    data: data
  })
}

// 修改企业部门信息
export function updateDeptInfo(data) {
  return request({
    url: '/cost/deptInfo',
    method: 'put',
    data: data
  })
}

// 删除企业部门信息
export function delDeptInfo(id) {
  return request({
    url: '/cost/deptInfo/' + id,
    method: 'delete'
  })
}

// 导出企业部门信息
export function exportDeptInfo(query) {
  return excelDownLoad('/cost/deptInfo/export',query)
}

export function costDeptInfoFindList(data) {
  return request({
    url: '/cost/deptInfo/findList',
    method: 'post',
    data: data||{}
  })
}

export function costDeptInfoTreeselect(data) {
  return request({
    url: '/cost/deptInfo/treeselect',
    method: 'post',
    data: data||{}
  })
}

// 查询企业部门信息
export function getDeptNameInfo(companyId,itemNo) {
  return request({
    url: '/cost/deptInfo/getDeptNameByCompanyId',
    method: 'get',
    params: { companyId: companyId, itemNo: itemNo }
  })
}
