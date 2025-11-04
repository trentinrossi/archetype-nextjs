'use client';

import React, { useEffect, useState } from 'react';
import { Account } from '@/types/account';
import { Table, Button, Select, Modal } from '@/components/ui';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';

const AccountsPage: React.FC = () => {
  const router = useRouter();
  const [accounts, setAccounts] = useState<Account[]>([]);
  const [filteredAccounts, setFilteredAccounts] = useState<Account[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [statusFilter, setStatusFilter] = useState<string>('all');
  const [groupIdFilter, setGroupIdFilter] = useState<string>('all');
  const [uniqueGroupIds, setUniqueGroupIds] = useState<string[]>([]);
  const [deleteModalOpen, setDeleteModalOpen] = useState(false);
  const [accountToDelete, setAccountToDelete] = useState<string | null>(null);
  const [deleting, setDeleting] = useState(false);

  const fetchAccounts = async () => {
    try {
      setLoading(true);
      setError(null);
      const data = await accountService.getAccounts();
      setAccounts(data);
      setFilteredAccounts(data);

      const groupIds = Array.from(new Set(data.map(account => account.groupId))).sort();
      setUniqueGroupIds(groupIds);
    } catch (err) {
      console.error('Error fetching accounts:', err);
      setError(err instanceof Error ? err.message : 'An unexpected error occurred');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchAccounts();
  }, []);

  useEffect(() => {
    let filtered = [...accounts];

    if (statusFilter !== 'all') {
      filtered = filtered.filter(account => account.activeStatus === statusFilter);
    }

    if (groupIdFilter !== 'all') {
      filtered = filtered.filter(account => account.groupId === groupIdFilter);
    }

    setFilteredAccounts(filtered);
  }, [statusFilter, groupIdFilter, accounts]);

  const handleViewDetails = (accountId: string) => {
    router.push(`/accounts/${accountId}`);
  };

  const handleEdit = (accountId: string) => {
    router.push(`/accounts/${accountId}/edit`);
  };

  const handleDeleteClick = (accountId: string) => {
    setAccountToDelete(accountId);
    setDeleteModalOpen(true);
  };

  const handleDeleteConfirm = async () => {
    if (!accountToDelete) return;

    try {
      setDeleting(true);
      await accountService.deleteAccount(accountToDelete);
      setAccounts(prev => prev.filter(account => account.accountId !== accountToDelete));
      setDeleteModalOpen(false);
      setAccountToDelete(null);
    } catch (err) {
      console.error('Error deleting account:', err);
      setError(err instanceof Error ? err.message : 'Failed to delete account');
    } finally {
      setDeleting(false);
    }
  };

  const handleDeleteCancel = () => {
    setDeleteModalOpen(false);
    setAccountToDelete(null);
  };

  const handleCreateNew = () => {
    router.push('/accounts/new');
  };

  const formatCurrency = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const formatDate = (dateString: string): string => {
    if (!dateString) return 'N/A';
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
    });
  };

  const columns = [
    { key: 'accountId', label: 'Account ID' },
    { key: 'activeStatus', label: 'Status' },
    { key: 'currentBalance', label: 'Current Balance' },
    { key: 'creditLimit', label: 'Credit Limit' },
    { key: 'cashCreditLimit', label: 'Cash Credit Limit' },
    { key: 'openDate', label: 'Open Date' },
    { key: 'groupId', label: 'Group ID' },
    { key: 'actions', label: 'Actions' },
  ];

  const tableData = filteredAccounts.map(account => ({
    accountId: <span className="font-medium text-gray-900">{account.accountId}</span>,
    activeStatus: (
      <span
        className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
          account.activeStatus === 'Y'
            ? 'bg-green-100 text-green-800'
            : 'bg-red-100 text-red-800'
        }`}
      >
        {account.activeStatus === 'Y' ? 'Active' : 'Inactive'}
      </span>
    ),
    currentBalance: <span className="text-gray-900">{formatCurrency(account.currentBalance)}</span>,
    creditLimit: <span className="text-gray-900">{formatCurrency(account.creditLimit)}</span>,
    cashCreditLimit: <span className="text-gray-900">{formatCurrency(account.cashCreditLimit)}</span>,
    openDate: <span className="text-gray-900">{formatDate(account.openDate)}</span>,
    groupId: <span className="text-gray-900">{account.groupId}</span>,
    actions: (
      <div className="flex gap-2">
        <Button
          variant="secondary"
          size="sm"
          onClick={() => handleViewDetails(account.accountId)}
        >
          View
        </Button>
        <Button
          variant="secondary"
          size="sm"
          onClick={() => handleEdit(account.accountId)}
        >
          Edit
        </Button>
        <Button
          variant="danger"
          size="sm"
          onClick={() => handleDeleteClick(account.accountId)}
        >
          Delete
        </Button>
      </div>
    ),
  }));

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading accounts...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="text-red-600 text-xl mb-4">Error</div>
          <p className="text-gray-600 mb-4">{error}</p>
          <Button onClick={fetchAccounts}>Retry</Button>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Accounts</h1>
        <p className="text-gray-600">Manage and view all customer accounts</p>
      </div>

      <div className="mb-6 flex flex-wrap gap-4 items-end">
        <div className="flex-1 min-w-[200px]">
          <label htmlFor="status-filter" className="block text-sm font-medium text-gray-700 mb-1">
            Filter by Status
          </label>
          <Select
            id="status-filter"
            value={statusFilter}
            onChange={(e) => setStatusFilter(e.target.value)}
            options={[
              { value: 'all', label: 'All Statuses' },
              { value: 'Y', label: 'Active' },
              { value: 'N', label: 'Inactive' },
            ]}
          />
        </div>

        <div className="flex-1 min-w-[200px]">
          <label htmlFor="group-filter" className="block text-sm font-medium text-gray-700 mb-1">
            Filter by Group ID
          </label>
          <Select
            id="group-filter"
            value={groupIdFilter}
            onChange={(e) => setGroupIdFilter(e.target.value)}
            options={[
              { value: 'all', label: 'All Groups' },
              ...uniqueGroupIds.map(groupId => ({
                value: groupId,
                label: groupId,
              })),
            ]}
          />
        </div>

        <div className="flex-shrink-0">
          <Button onClick={handleCreateNew}>Create New Account</Button>
        </div>
      </div>

      {filteredAccounts.length === 0 ? (
        <div className="text-center py-12 bg-gray-50 rounded-lg">
          <svg
            className="mx-auto h-12 w-12 text-gray-400"
            fill="none"
            viewBox="0 0 24 24"
            stroke="currentColor"
            aria-hidden="true"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"
            />
          </svg>
          <h3 className="mt-2 text-sm font-medium text-gray-900">No accounts found</h3>
          <p className="mt-1 text-sm text-gray-500">
            {accounts.length === 0
              ? 'Get started by creating a new account.'
              : 'Try adjusting your filters to see more results.'}
          </p>
          {accounts.length === 0 && (
            <div className="mt-6">
              <Button onClick={handleCreateNew}>Create New Account</Button>
            </div>
          )}
        </div>
      ) : (
        <div className="bg-white shadow-md rounded-lg overflow-hidden">
          <Table columns={columns} data={tableData} />
          <div className="px-6 py-4 bg-gray-50 border-t border-gray-200">
            <p className="text-sm text-gray-700">
              Showing <span className="font-medium">{filteredAccounts.length}</span> of{' '}
              <span className="font-medium">{accounts.length}</span> accounts
            </p>
          </div>
        </div>
      )}

      <Modal
        isOpen={deleteModalOpen}
        onClose={handleDeleteCancel}
        title="Delete Account"
      >
        <div className="mt-2">
          <p className="text-sm text-gray-500">
            Are you sure you want to delete account <strong>{accountToDelete}</strong>? This action cannot be undone.
          </p>
        </div>
        <div className="mt-6 flex justify-end gap-3">
          <Button
            variant="secondary"
            onClick={handleDeleteCancel}
            disabled={deleting}
          >
            Cancel
          </Button>
          <Button
            variant="danger"
            onClick={handleDeleteConfirm}
            disabled={deleting}
          >
            {deleting ? 'Deleting...' : 'Delete'}
          </Button>
        </div>
      </Modal>
    </div>
  );
};

export default AccountsPage;
