import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'


export function getCompanyList(data) {
  return request({
    url: '/project/companyinfo/listCompany',
    method: 'get',
    data: data
  })
}
// 查询企业信息详细
export function getCompanyinfo(id) {
  return request({
    url: '/project/companyinfo/' + id,
    method: 'get'
  })
}

// 新增企业信息
export function addCompanyinfo(data) {
  return request({
    url: '/project/companyinfo',
    method: 'post',
    data: data
  })
}

// 修改企业信息
export function updateCompanyinfo(data) {
  return request({
    url: '/project/companyinfo',
    method: 'put',
    data: data
  })
}

// 删除企业信息
export function delCompanyinfo(id) {
  return request({
    url: '/project/companyinfo/' + id,
    method: 'delete'
  })
}

// 导出企业信息
export function exportCompanyinfo(query) {
  return excelDownLoad('/project/companyinfo/export',query)
}

export function findListCompanyinfo(data) {
  return request({
    url: '/project/companyinfo/findList',
    method: 'post',
    data: data||{}
  })
}

export function projectCompanyinfoFindList(data) {
  return request({
    url: '/project/companyinfo/findList',
    method: 'post',
    data: data||{}
  })
}

// 天眼查查询客户信息
export function getTianyanCompanyinfo(name) {
  return request({
    url: '/project/companyinfo/selectCompanyInfoByTianYan',
    params: { name: name },
    method: 'get'
  })
}

// 获取邮政编码
export function listPostCode(data) {
  return request({
    url: 'https://www.ems.com.cn/ems-web/inquiryPostalCode/showCode',
    method: 'post',
    data: data
  })
}