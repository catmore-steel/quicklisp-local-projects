import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询预算研发项目拟定;详细
export function getGeneralBudget(id) {
  return request({
    url: '/cost/generalBudget/' + id,
    method: 'get'
  })
}

// 新增预算研发项目拟定;
export function addGeneralBudget(data) {
  return request({
    url: '/cost/generalBudget/save',
    method: 'post',
    data: data,
  })
}

// 修改预算研发项目拟定;
export function updateGeneralBudget(data) {
  return request({
    url: '/cost/generalBudget',
    method: 'put',
    data: data
  })
}

// 删除预算研发项目拟定;
export function delGeneralBudget(id) {
  return request({
    url: '/cost/generalBudget/' + id,
    method: 'delete'
  })
}

// 导出预算研发项目拟定;
export function exportGeneralBudget(query) {
  return excelDownLoad('/cost/generalBudget/export', query)
}

// 查询初步拟定研发费用占比饼图
export function getDevelopFeeChartByCompanyIdAndItemNo(companyId, itemNo) {
  return request({
    url: '/cost/generalBudget/getDevelopFeeChartByCompanyIdAndItemNo',
    method: 'get',
    params: { companyId: companyId, itemNo: itemNo }
  })
}

// 初步拟定研发费用占比詳細信息
export function getFeeProposedDetail(companyId, itemNo) {
  return request({
    url: '/cost/generalBudget/getFeeProposedDetail',
    method: 'get',
    params: { companyId: companyId, itemNo: itemNo }
  })
}
