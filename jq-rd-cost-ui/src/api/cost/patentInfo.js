import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询企业知识产权信息详细
export function getPatentInfo(id) {
  return request({
    url: '/cost/patentInfo/' + id,
    method: 'get'
  })
}

// 新增企业知识产权信息
export function addPatentInfo(data) {
  return request({
    url: '/cost/patentInfo',
    method: 'post',
    data: data
  })
}

// 修改企业知识产权信息
export function updatePatentInfo(data) {
  return request({
    url: '/cost/patentInfo',
    method: 'put',
    data: data
  })
}

// 删除企业知识产权信息
export function delPatentInfo(id) {
  return request({
    url: '/cost/patentInfo/' + id,
    method: 'delete'
  })
}

// 导出企业知识产权信息
export function exportPatentInfo(query) {
  return excelDownLoad('/cost/patentInfo/export',query)
}

// 通过客户，项目编号，知产编号获取知产信息
export function getPatentInfoByCompanyIdItemNoPatentCode(data) {
  return request({
    url: '/cost/patentInfo/getPatentInfoByCompanyIdItemNoPatentCode',
    method: 'get',
    params: data
  })
}

//获取知产类别下拉数据
export function getPatentTypeSelect() {
  return request({
    url: '/conf/patentType/getPatentTypeSelect',
    method: 'get'
  })
}

//拟定研发项目初始化数据
export function workProjectInfoInit(params) {
  return request({
    url: '/cost/patentInfo/workProjectInfoInit',
    method: 'get',
    params: params
  })
}

//拟定研发项目提交
export function workProjectSubmit(data) {
  return request({
    url: '/cost/patentInfo/workProjectSubmit',
    method: 'post',
    data: data
  })
}




