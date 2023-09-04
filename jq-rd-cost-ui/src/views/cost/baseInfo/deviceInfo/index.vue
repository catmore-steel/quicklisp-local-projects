<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="handleAdd"
                   v-hasPermi="['cost:deviceInfo:add']"
        >
          新增
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="danger" icon="el-icon-delete" :disabled="multiple" @click="handleDelete"
                   v-hasPermi="['cost:deviceInfo:remove']"
        >
          删除
        </el-button>
      </el-col>
    </el-row>

    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :showSearch.sync="showSearch" @handleSelectionChange="handleSelectionChange">
      <template slot="search">
        <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="100px">
          <el-row>
            <el-col :span="6">
              <el-form-item label="设备编码" prop="deviceNo">
                <el-input
                  v-model="queryParams.deviceNo"
                  placeholder="请输入设备编码"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="设备名称" prop="deviceName">
                <el-input
                  v-model="queryParams.deviceName"
                  placeholder="请输入设备名称"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="设备类型" prop="deviceType">
                <el-select v-model="queryParams.deviceType" placeholder="请选择设备类型" class="form-control" clearable filterable>
                  <el-option
                    v-for="dict in deviceTypeOptions"
                    :key="dict.dictValue"
                    :label="dict.dictLabel"
                    :value="dict.dictValue"
                  ></el-option>
                </el-select>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="规格型号" prop="deviceSpecs">
                <el-input
                  v-model="queryParams.deviceSpecs"
                  placeholder="请输入规格型号"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"
                />
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="6">
              <el-form-item label="取得日期" prop="getDate">
                <el-date-picker
                  clearable size="small"
                  v-model="queryParams.getDate"
                  class="form-control"
                  type="date"
                  value-format="yyyy-MM-dd"
                  placeholder="选择取得日期"
                >
                </el-date-picker>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="使用部门" prop="getDeptId">
                <treeselect v-model="queryParams.getDeptId" :options="deptOptions" :multiple="false" placeholder="请选择使用部门" class="form-control"/>
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
      </template>
      <template slot="deviceNo" slot-scope="{scope}">
        <a @click="deviceUpdate(scope.row)" v-copy>
          {{ scope.row.deviceNo }}
        </a>
      </template>
    </jq-table>

    <!---企业设备信息查看详情 -->
    <deviceInfo-detail :show="deviceCard.show" v-model="deviceCard.key" @handleClose="deviceClose"/>

  </div>
</template>

<script>
import JqTableMixin from '@/mixin/JqTable'
import deviceInfoDetail from '../common/deviceInfoDetail'
import { getDeptNameInfo } from '@/api/cost/deptInfo'
import { delDeviceInfo } from '@/api/cost/deviceInfo'

export default {
  name: 'deviceInfo',
  mixins: [JqTableMixin],
  provide() {
    return {
      handleQuery: this.handleQuery
    }
  },
  data() {
    return {
      // 选中数组
      ids: [],
      // 非单个禁用
      single: true,
      // 非多个禁用
      multiple: true,
      // 显示搜索条件
      showSearch: true,
      // 是否显示导出弹出层
      exportOpen: false,
      //导入弹出层title
      importTitle: '导入',
      // 是否显示导出弹出层
      importOpen: false,
      // 客户名称
      companyId: this.$store.state.item.companyId,
      // 申报项目
      itemNo: this.$store.state.item.itemNo,
      // 使用部门下拉框
      deptOptions: [],
      // 设备类型字典
      deviceTypeOptions: [],
      // 查询参数
      queryParams: {},
      //表格配置数据
      tableConfig: {
        url: '/cost/deviceInfo/list',
        method: 'get',
        queryParams: null,
        orders: 'base_device_info.update_time desc',
        exportUrl: '/cost/deviceInfo/export',
        globalFlg: true,
        superSearch: {
          keyPlaceholder: '设备编码/设备名称',
          radioSearch: false,
          radioData: [{ label: '标签一', value: 1 }, { label: '标签二', value: 2 }]
        }
      },
      deviceCard: {
        show: false,
        key: null
      }
    }
  },
  components: {
    deviceInfoDetail
  },
  // watch: {
  //   '$store.state.item.itemNo'(newVal, oldVal) {
  //     this.$nextTick(() => {
  //       this.itemNo = this.$store.state.item.itemNo
  //       this.companyId = this.$store.state.item.companyId
  //     })
  //   }
  // },
  created() {
    //设备类型字典
    this.getDicts('device_type').then(response => {
      this.deviceTypeOptions = response.data
    })
    //使用部门下拉框
    getDeptNameInfo(this.$store.state.item.companyId, this.$store.state.item.itemNo).then(response => {
      this.deptOptions = response.data
    })
  },
  methods: {
    /** 搜索按钮操作 */
    handleQuery() {
      this.tableConfig.queryParams = this.queryParams
      this.getJqTableData()
    },

    /** 多选框选中数据 */
    handleSelectionChange(selection) {
      this.ids = selection.map(item => item.id)
      this.single = selection.length !== 1
      this.multiple = !selection.length
    },

    /** 新增按钮操作 */
    handleAdd() {
      this.deviceCard = {
        show: true,
        key: null
      }
    },

    /** 查看企业设备信息 */
    deviceUpdate(row) {
      this.deviceCard = {
        show: true,
        key: row.id//此处id需替换为业务表唯一索引（非id）
      }
    },

    /** 关闭企业设备信息查看弹框 */
    deviceClose() {
      this.deviceCard = {
        show: false,
        row: null
      }
    },

    /** 删除按钮操作 */
    handleDelete(row) {
      const ids = row.id || this.ids
      this.$confirm('是否确认删除企业设备信息编号为"' + ids + '"的数据项?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return delDeviceInfo(ids)
      }).then(() => {
        this.getJqTableData()
        this.msgSuccess('删除成功')
        this.handleClose()
        this.handleQuery()
      }).catch(() => {
      })
    }

  }
}
</script>
