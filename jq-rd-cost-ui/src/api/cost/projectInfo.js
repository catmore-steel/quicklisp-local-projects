import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询研发项目管理详细
export function getProjectInfo(id) {
  return request({
    url: '/cost/projectInfo/' + id,
    method: 'get'
  })
}

// 新增研发项目管理
export function addProjectInfo(data) {
  return request({
    url: '/cost/projectInfo',
    method: 'post',
    data: data
  })
}

// 修改研发项目管理
export function updateProjectInfo(data) {
  return request({
    url: '/cost/projectInfo',
    method: 'put',
    data: data
  })
}

// 删除研发项目管理
export function delProjectInfo(id) {
  return request({
    url: '/cost/projectInfo/' + id,
    method: 'delete'
  })
}

// 导出研发项目管理
export function exportProjectInfo(query) {
  return excelDownLoad('/cost/projectInfo/export',query)
}

//获取某个客户、某个项目的研发项目信息
export function getProjectByCompanyIdItemNo(companyId, itemNo) {
  return request({
    url: '/cost/projectInfo/getProjectByCompanyIdItemNo?companyId=' + companyId + '&itemNo=' + itemNo,
    method: 'get'
  })
}

// 通过客户，项目编号，知产编号获取知产信息
export function getProjectInfoByCompanyIdItemNoProjectNo(data) {
  return request({
    url: '/cost/projectInfo/getProjectInfoByCompanyIdItemNoProjectNo',
    method: 'get',
    params: data
  })
}

//
export function projectStagesSubmit(data) {
  return request({
    url: '/cost/projectInfo/projectStagesSubmit',
    method: 'post',
    data: data
  })
}

//通过项目id获取项目研发阶段数据
export function getProjectStagesList(projectId) {
  return request({
    url: '/cost/projectInfo/getProjectStagesList?projectId=' + projectId,
    method: 'get'
  })
}

//通过项目id获取项目研发预算数据
export function getProjectBudgetList(projectId) {
  return request({
    url: '/cost/projectInfo/getProjectBudgetList?projectId=' + projectId,
    method: 'get'
  })
}

//通过项目id获取项目研发预算数据
export function getProjectStagesSelect(projectId) {
  return request({
    url: '/cost/projectInfo/getProjectStagesSelect?projectId=' + projectId,
    method: 'get'
  })
}

//获取研发项目各个阶段参与人员投入费用数据
export function getProjectStagesPutIntoDetailList(projectId, feeTypeName, projectStageId) {
  return request({
    url: '/cost/projectInfo/getProjectStagesPutIntoDetailList?projectId=' + projectId + '&feeTypeName=' + feeTypeName + '&projectStageId=' + projectStageId,
    method: 'get'
  })
}

//获取研发项目各个阶段投入费用数据
export function getProjectStagePutIntoList(projectId, feeTypeName) {
  return request({
    url: '/cost/projectInfo/getProjectStagesPutIntoList?projectId=' + projectId + '&feeTypeName=' + feeTypeName,
    method: 'get'
  })
}

//获取研发项目阶段每个月投入费用数据
export function getProjectStageFeeList(data) {
  return request({
    url: '/cost/projectInfo/getProjectStageFeeList',
    method: 'get',
    params: data
  })
}

//费用分摊提交
export function submitFeeShare(data) {
  return request({
    url: '/cost/projectInfo/submitFeeShare',
    method: 'post',
    data: data
  })
}

//获取年度研发费用投入情况
export function getYearProjectPutIntoInfo(data) {
  return request({
    url: '/cost/projectInfo/getYearProjectPutIntoInfo',
    method: 'get',
    params: data
  })
}

//获取年度研发费用投入情况
export function getProjectInitProportion(data) {
  return request({
    url: '/cost/projectInfo/getProjectInitProportion',
    method: 'get',
    params: data
  })
}

//研发项目初始占比提交
export function submitProjectRate(data) {
  return request({
    url: '/cost/projectInfo/submitProjectRate',
    method: 'post',
    data: data
  })
}

//获取研发项目初始占比echarts数据
export function getProjectInitProportionEcharts(data) {
  return request({
    url: '/cost/projectInfo/getProjectInitProportionEcharts',
    method: 'get',
    params: data
  })
}

//项目预算提交
export function submitProjectBudget(data) {
  return request({
    url: '/cost/projectInfo/submitProjectBudget',
    method: 'post',
    data: data
  })
}


//研发项目附件上传
export function uploadProjectAttach(data) {
  return request({
    url: '/cost/projectInfo/uploadProjectAttach',
    method: 'post',
    data: data
  })
}

//一键分摊
export function oneKeyShare(data) {
  return request({
    url: '/cost/projectInfo/oneKeyShare',
    method: 'get',
    params: data
  })
}

// 查询所有研发项目
export function getAllProject(data) {
  return request({
    url: '/cost/projectInfo/getAllProject',
    method: 'get',
    params: data
  })
}

//辅助账汇总表
export function getSummaryTableList(data) {
  return request({
    url: '/cost/projectInfo/getSummaryTableList',
    method: 'get',
    params: data
  })
}

// 人员工时记录list
export function listStageUser(data) {
  return request({
    url: '/cost/projectInfo/listStageUser',
    method: 'get',
    params: data
  })
}

// 一键分摊人员工时记录
export function shareStageUser(data) {
  return request({
    url: '/cost/projectInfo/shareStageUser',
    method: 'post',
    params: data
  })
}

// 查询人员工时记录
export function getStageUser(id) {
  return request({
    url: '/cost/projectInfo/getStageUser/' + id,
    method: 'get'
  })
}

// 人员工时分摊list
export function listHoursShare(data) {
  return request({
    url: '/cost/projectInfo/listHoursShare',
    method: 'get',
    params: data
  })
}

// 人员工时分摊查询
export function getHoursShare(data) {
  return request({
    url: '/cost/projectInfo/getHoursShare',
    method: 'get',
    params: data
  })
}

