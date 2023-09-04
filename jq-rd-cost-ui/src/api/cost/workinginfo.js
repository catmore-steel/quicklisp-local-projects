import request from '@/utils/request'

//选中考勤类型和工作日标准工时后的保存操作
export function saveLegalHoliday(data) {
  return request({
    url: '/cost/workinginfo/save', method: 'post', data: data
  })
}

// 查询公司工作情况信息
export function getWorkingInfoByCompanyIdAndItemNo(companyId, itemNo) {
  return request({
    url: '/cost/workinginfo/getWorkingInfoByCompanyIdAndItemNo',
    method: 'get',
    params: {companyId: companyId, itemNo: itemNo}
  })
}

// 查询公司工作情况信息数组
export function getWorkingInfo(companyId, itemNo) {
  return request({
    url: '/cost/workinginfo/getWorkingInfo',
    method: 'get',
    params: {companyId: companyId, itemNo: itemNo}
  })
}

// 通过客户名称和申报项目和年月查询公司工作情况明细
export function getWorkingDetailByMonth(companyId, itemNo, yearMonth) {
  return request({
    url: '/cost/workinginfo/getWorkingDetailByMonth',
    method: 'get',
    params: {companyId: companyId, itemNo: itemNo, yearMonth: yearMonth}
  })
}

// 保存月公司工作情况明细
export function updateWorkingDetail(data) {
  return request({
    url: '/cost/workinginfo/updateWorkingDetail', method: 'post', data: data
  })
}
