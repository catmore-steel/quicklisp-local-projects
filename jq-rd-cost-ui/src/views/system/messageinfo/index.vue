<template>
  <div class="app-container">
    <el-form :model="queryParams" ref="queryForm" :inline="true"  label-width="100px">
    <el-row>
        <el-col :span="6">
          <el-form-item label="发送人" prop="sendUser">
            <el-input
              v-model="queryParams.sendUser"
              placeholder="请输入发送人"
              clearable

              class="form-control"
              @keyup.enter.native="handleQuery"
            />
          </el-form-item>
        </el-col>
        <el-col :span="6">
          <el-form-item label="发送时间" prop="sendTime">
            <el-date-picker  clearable
              v-model="queryParams.sendTime"
              class="form-control"
              type="date"
              value-format="yyyy-MM-dd"
              placeholder="选择发送时间">
            </el-date-picker>
          </el-form-item>
        </el-col>
            <el-col :span="6">
              <el-form-item>
                <el-button type="cyan" icon="el-icon-search"  @click="handleQuery">搜索</el-button>
                <el-button icon="el-icon-refresh"  @click="resetQuery">重置</el-button>
              </el-form-item>
            </el-col>
        </el-row>
    </el-form>



    <el-table v-loading="loading" :data="messageinfoList" @selection-change="handleSelectionChange"  @sort-change="sortChange"  :default-sort = "defaultSort[0]">
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="发送时间" prop="id" />
      <el-table-column label="发送人" prop="sendUser" />
      <el-table-column label="发送时间" prop="sendTime" width="180">
        <template slot-scope="scope">
          <span>{{ parseTime(scope.row.sendTime, '{y}-{m}-{d}') }}</span>
        </template>
      </el-table-column>
      <el-table-column label="发送内容" prop="content" />
      <el-table-column label="操作" :sortable='false' class-name="small-padding fixed">
        <template slot-scope="scope">
          <el-button

            type="text"
            icon="el-icon-edit"
            @click="handleUpdate(scope.row)"
            v-hasPermi="['system:messageinfo:edit']"
          >查看</el-button>
        </template>
      </el-table-column>
    </el-table>

    <pagination
      v-show="total>0"
      :total="total"
      :page.sync="queryParams.pageNum"
      :limit.sync="queryParams.pageSize"
      @pagination="getList"
    />



    <!-- 添加或修改系统消息对话框 -->
    <jq-dialog :title="title" :visible.sync="open" width="800px" append-to-body>
      <el-form ref="form" :model="form" :rules="rules" label-width="100px">
        <el-row>
            <el-col :span="12">
                <el-form-item label="发送人" prop="sendUser">
                  <el-input v-model="form.sendUser" placeholder="请输入发送人" />
                </el-form-item>
            </el-col>
            <el-col :span="12">
                <el-form-item label="发送时间" prop="sendTime">
                  <el-date-picker clearable
                    v-model="form.sendTime"
                    class="form-control"
                    type="date"
                    value-format="yyyy-MM-dd"
                    placeholder="选择发送时间">
                  </el-date-picker>
                </el-form-item>
            </el-col>
        </el-row>
        <el-row>
            <el-col :span="12">
                <el-form-item label="发送内容">
                  <editor v-model="form.content" :min-height="192"/>
                </el-form-item>
            </el-col>
        </el-row>
      </el-form>
      <div slot="footer" >
        <el-button @click="cancel">关 闭</el-button>
      </div>
    </jq-dialog>

      <!---系统消息导出组件 -->
      <export-dialog :show.sync="exportOpen"
                     :type.sync="queryParams.exportType"
                     @handleExport="handleExport"></export-dialog>

  </div>
</template>

<script>
import { listMessageinfo, getMessageinfo, delMessageinfo, addMessageinfo, updateMessageinfo, exportMessageinfo } from "@/api/system/messageinfo";
import Editor from '@/components/Editor';


export default {
  name: "Messageinfo",
  components: { Editor },
  data() {
    return {
      // 遮罩层
      loading: true,
      // 选中数组
      ids: [],
      // 非单个禁用
      single: true,
      // 非多个禁用
      multiple: true,
      // 显示搜索条件
      showSearch: true,
      // 总条数
      total: 0,
      // 系统消息表格数据
      messageinfoList: [],
      // 弹出层标题
      title: "",
      // 是否显示弹出层
      open: false,
      // 是否显示数据
      exportTitle: "导出警告",
      // 是否显示导出弹出层
      exportOpen: false,
      //查询排序参数
      defaultSort : [],
      // 查询参数
      queryParams: {
        pageNum: 1,
        pageSize: 15,
        exportType:1,
        ids: null,
        orders: null,
        sendUser: null,
        sendTime: null,
      },
      // 表单参数
      form: {},
      // 表单校验
      rules: {
      }
    };
  },
  created() {
    this.getList();
  },
  methods: {
    /** 查询系统消息列表 */
    getList() {
      this.loading = true;
      //设置默认排序
      if (this.isNullOrEmpty(this.queryParams.orders)){
        this.queryParams.orders = this.sortByColumn(this.defaultSort);
      }
      listMessageinfo(this.queryParams).then(response => {
        this.messageinfoList = response.rows;
        this.total = response.total;
        this.loading = false;
      });
    },
    // 取消按钮
    cancel() {
      this.open = false;
      this.reset();
    },
    // 表单排序
    sortChange(column) {
      this.queryParams.orders = this.sortByColumn(column);
      this.getList();
    },
    // 表单重置
    reset() {
      this.form = {
          //附件列表
        attachmentList: [],
        id: null,
        sendUser: null,
        sendTime: null,
        content: null,
        receiveUser: null,
        receiveDate: null,
        status: "0",
        warnType: null,
        messageType: null,
        router: null,
        createUserId: null,
        createDate: null
      };
      this.resetForm("form");
    },
    /** 搜索按钮操作 */
    handleQuery() {
      this.queryParams.pageNum = 1;
      this.getList();
    },
    /** 重置按钮操作 */
    resetQuery() {
      this.resetForm("queryForm");
      this.handleQuery();
    },
    // 多选框选中数据
    handleSelectionChange(selection) {
      this.ids = selection.map(item => item.id)
      this.single = selection.length!==1
      this.multiple = !selection.length
    },
    /** 新增按钮操作 */
    handleAdd() {
      this.reset();
      this.open = true;
      this.title = "添加系统消息";
    },
    /** 修改按钮操作 */
    handleUpdate(row) {
      this.reset();
      const id = row.id || this.ids
      getMessageinfo(id).then(response => {
        this.form = response.data;
        this.open = true;
        this.title = "修改系统消息";
      });
    },
    /** 提交按钮 */
    submitForm() {
      this.$refs["form"].validate(valid => {
        if (valid) {
          if (this.form.id != null) {
            updateMessageinfo(this.form).then(response => {
              this.msgSuccess("修改成功");
              this.open = false;
              this.getList();
            });
          } else {
            addMessageinfo(this.form).then(response => {
              this.msgSuccess("新增成功");
              this.open = false;
              this.getList();
            });
          }
        }
      });
    },
    /** 删除按钮操作 */
    handleDelete(row) {
      const ids = row.id || this.ids;
      this.$confirm('是否确认删除系统消息编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function() {
          return delMessageinfo(ids);
        }).then(() => {
          this.getList();
          this.msgSuccess("删除成功");
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
          exportMessageinfo(queryParams);
          this.exportOpen = false;
      },





  }
};
</script>
