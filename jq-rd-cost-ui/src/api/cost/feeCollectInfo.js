import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

export function costAccountInfoList(query) {
	  return request({
	    url: '/cost/feeCollectInfo/baseAccountInfo/list',
	    method: 'get',
	    params: query
	  })
}


export function saveForReqVO(data) {
	return request({
	  url: '/cost/feeCollectInfo/saveForReqVO',
	  method: 'post',
	  data: data
	})
  }


export function costAccountInfoStatistics(query) {
	return request({
	  url: '/cost/feeCollectInfo/baseAccountInfo/statistics',
	  method: 'get',
	  params: query
	})
}  