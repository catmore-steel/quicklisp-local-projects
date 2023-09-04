<template>
  <div class="app-container">
    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button type="primary" icon="el-icon-plus" @click="handleAdd"
                   v-hasPermi="['project:baseItemInfo:add']">
          新增
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button type="danger" icon="el-icon-delete" :disabled="multiple" @click="handleDelete"
                 v-hasPermi="['project:baseItemInfo:remove']">
          删除
        </el-button>
      </el-col>
      <el-button type="warning" icon="el-icon-download" @click="confirmExport"
                 v-hasPermi="['project:baseItemInfo:export']">
        导出
      </el-button>
    </el-row>

    <jq-table :config="tableConfig" :queryParams.sync="queryParams" ref="JqTableRef" :showSearch.sync="showSearch"
                  @handleSelectionChange="handleSelectionChange">
      <template slot="search">
        <el-form :model="queryParams" ref="queryForm" :inline="true" label-width="100px">
          <el-row>
            <el-col :span="6">
              <el-form-item label="所属客户" prop="companyId">
                <el-input
                  v-model="queryParams['baseCompanyInfo.companyName']"
                  placeholder="请输入所属客户"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="项目编号" prop="itemNo">
                <el-input
                  v-model="queryParams.itemNo"
                  placeholder="请输入项目编号"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="项目进度" prop="itemStatus">
                <el-select v-model="queryParams.itemStatus" placeholder="请选择项目进度" class="form-control" clearable filterable>
                  <el-option v-for="dict in projectStatusList" :key="dict.dictValue" :label="dict.dictLabel" :value="dict.dictValue" ></el-option>
                </el-select>
              </el-form-item>
            </el-col>
            <el-col :span="6">
              <el-form-item label="客户联系人" prop="contactName">
                <el-input
                  v-model="queryParams.contactName"
                  placeholder="请输入客户联系人"
                  clearable
                  class="form-control"
                  @keyup.enter.native="handleQuery"/>
              </el-form-item>
            </el-col>

          </el-row>
          <el-row>
            <el-col :span="6">
              <el-form-item label="申报年份" prop="applyDate">
              <el-date-picker clearable size="small" v-model="queryParams.applyDate" class="form-control" type="year" value-format="yyyy" placeholder="请输入申报年份">
            </el-date-picker>
              </el-form-item>
            </el-col>
          </el-row>

        </el-form>
      </template>
      <template slot="itemNo" slot-scope="{scope}">
        <a @click="infoUpdate(scope.row)" style="color: #1890ff">
          {{ scope.row.itemNo }}
          <i class="el-icon-document-copy" title="点击复制" @click.stop="copyNodeValue($event,scope.row.itemNo)"/>
        </a>
      </template>
    </jq-table>


    <!---项目管理;导出组件 -->
    <export-dialog :show.sync="exportOpen" :type.sync="queryParams.exportType"
                   @handleExport="handleExport"/>

    <!---项目管理;查看详情 -->
    <info-detail :show="infoCard.show" v-model="infoCard.key" :row="row"
                    @handleClose="infoClose" @handleQuery="handleQuery"/>

  </div>
</template>

<script>
  import JqTableMixin from '@/mixin/JqTable'
  import infoDetail from '../common/infoDetail'
  import { delBaseItemInfo ,exportBaseItemInfo} from '@/api/project/baseItemInfo.js'
  import Clipboard from 'clipboard'
  export default {
    name: "info",
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
        // 是否显示数据
        exportTitle: "导出警告",
        // 是否显示导出弹出层
        exportOpen: false,
        // 查询参数
        queryParams: {
          ['baseCompanyInfo.companyName']:null
        },
        //表格配置数据
        tableConfig: {
            url: '/project/baseItemInfo/list',
          method: 'get',
          queryParams: null,
          orders: 'base_item_info.update_time desc',
          exportUrl: '/project/baseItemInfo/export',
          superSearch: {
            keyPlaceholder: '项目名称/申报年份/项目编号',
            radioSearch: false,
            radioData: []
          }
        },
        infoCard: {
          show: false,
          key: null
        },
        projectStatusList:[],
        row:{}
      }
    },
    components: {
      infoDetail
    },
    created() {
      //project_status
      this.getDicts('project_status').then(response => { this.projectStatusList = response.data });
    },
    methods: {
      /** 搜索按钮操作 */
      handleQuery() {
        this.tableConfig.queryParams = this.queryParams
        this.getJqTableData()
      },

      /** 重置按钮操作 */
      resetQuery() {
        this.resetForm("queryForm");
        this.handleQuery();
      },

      /** 多选框选中数据 */
      handleSelectionChange(selection) {
        this.ids = selection.map(item => item.id)
        this.single = selection.length !== 1
        this.multiple = !selection.length
      },

      /** 新增按钮操作 */
      handleAdd() {
        this.infoCard = {
          show: true,
          key: null
        }
      },

      /** 查看项目管理; */
      infoUpdate(row) {
        this.infoCard = {
          show: true,
          key: row.id//此处id需替换为业务表唯一索引（非id）
        }
        this.row=row;
      },

      /** 关闭项目管理;查看弹框 */
      infoClose() {
        this.infoCard = {
          show: false,
          row: null
        }
      },

      /** 删除按钮操作 */
      handleDelete(row) {
        const ids = row.id || this.ids;
        this.$confirm('是否确认删除项目管理;编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delBaseItemInfo(ids);
        }).then(() => {
          this.getJqTableData()
          this.msgSuccess("删除成功");
        }).catch(() => {
        })
      },

      /** 打开导出页面按钮 */
      confirmExport() {
        this.queryParams.exportType = 1;
        this.exportOpen = true;
      },

      /** 确认导出按钮操作 */
      handleExport() {
        if (0 === this.ids.length && 3 === this.queryParams.exportType) {
          this.msgError("未选中任何数据！");
          return
        }
        this.queryParams.ids = this.ids.join(",");
        let queryParams = this.queryParams;
        exportBaseItemInfo(queryParams);
        this.exportOpen = false;
      },
    copyNodeValue(e, text) {
      const clipboard = new Clipboard(e.target, {text: () => text})
      clipboard.on('success', e => {
        this.$message.success('复制成功!')
        // 释放内存
        clipboard.off('error')
        clipboard.off('success')
        clipboard.destroy()
      })
      // err
      clipboard.on('error', e => {
        this.$message.error('复制失败!')
        clipboard.off('error')
        clipboard.off('success')
        clipboard.destroy()
      })
      clipboard.onClick(e)
    },

    }
  }
</script>
