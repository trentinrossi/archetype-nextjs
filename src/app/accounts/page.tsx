'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { Account } from '@/types/account';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell, Button, Select } from '@/components/ui';

export default function AccountsPage() {
  const router = useRouter();
  const [accounts, setAccounts] = useState<Account[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [totalElements, setTotalElements] = useState(0);
  const [pageSize, setPageSize] = useState(20);
  const [filterType, setFilterType] = useState<'all' | 'active' | 'expired' | 'sequential'>('all');

  const fetchAccounts = useCallback(async () => {
    try {
      setLoading(true);
      setError(null);

      if (filterType === 'active') {
        const data = await accountService.getActiveAccounts();
        setAccounts(data);
        setTotalPages(1);
        setTotalElements(data.length);
      } else if (filterType === 'expired') {
        const data = await accountService.getExpiredAccounts();
        setAccounts(data);
        setTotalPages(1);
        setTotalElements(data.length);
      } else if (filterType === 'sequential') {
        const data = await accountService.getAccountsSequentially();
        setAccounts(data);
        setTotalPages(1);
        setTotalElements(data.length);
      } else {
        const response = await accountService.getAccounts(currentPage, pageSize, 'acctId,asc');
        setAccounts(response.content);
        setTotalPages(response.totalPages);
        setTotalElements(response.totalElements);
      }
    } catch (err) {
      setError('Failed to load accounts');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, [currentPage, pageSize, filterType]);

  useEffect(() => {
    fetchAccounts();
  }, [fetchAccounts]);

  const handleDelete = async (id: string) => {
    if (!confirm('Are you sure you want to delete this account?')) return;

    try {
      await accountService.deleteAccount(id);
      fetchAccounts();
    } catch (err) {
      alert('Failed to delete account');
      console.error(err);
    }
  };

  const handlePreviousPage = () => {
    if (currentPage > 0) {
      setCurrentPage(currentPage - 1);
    }
  };

  const handleNextPage = () => {
    if (currentPage < totalPages - 1) {
      setCurrentPage(currentPage + 1);
    }
  };

  const formatCurrency = (value: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(value);
  };

  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleDateString('en-US');
  };

  if (loading) return <div className="p-6">Loading accounts...</div>;
  if (error) return <div className="p-6 text-red-600">Error: {error}</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Account Management</h1>
        <Button onClick={() => router.push('/accounts/new')}>
          Create New Account
        </Button>
      </div>

      <div className="mb-4 flex gap-4 items-center">
        <div className="flex-1">
          <Select
            label="Filter Accounts"
            value={filterType}
            onChange={(e) => {
              setFilterType(e.target.value as 'all' | 'active' | 'expired' | 'sequential');
              setCurrentPage(0);
            }}
            options={[
              { value: 'all', label: 'All Accounts (Paginated)' },
              { value: 'active', label: 'Active Accounts Only' },
              { value: 'expired', label: 'Expired Accounts Only' },
              { value: 'sequential', label: 'Sequential Processing (BR-001)' },
            ]}
          />
        </div>
        {filterType === 'all' && (
          <div className="w-48">
            <Select
              label="Page Size"
              value={pageSize.toString()}
              onChange={(e) => {
                setPageSize(Number(e.target.value));
                setCurrentPage(0);
              }}
              options={[
                { value: '10', label: '10 per page' },
                { value: '20', label: '20 per page' },
                { value: '50', label: '50 per page' },
                { value: '100', label: '100 per page' },
              ]}
            />
          </div>
        )}
      </div>

      <div className="mb-4 text-sm text-gray-600">
        Showing {accounts.length} of {totalElements} accounts
        {filterType === 'all' && ` (Page ${currentPage + 1} of ${totalPages})`}
      </div>

      <Table>
        <TableHeader>
          <TableRow>
            <TableHead>Account ID</TableHead>
            <TableHead>Status</TableHead>
            <TableHead>Current Balance</TableHead>
            <TableHead>Credit Limit</TableHead>
            <TableHead>Available Credit</TableHead>
            <TableHead>Open Date</TableHead>
            <TableHead>Expired</TableHead>
            <TableHead>Actions</TableHead>
          </TableRow>
        </TableHeader>
        <TableBody>
          {accounts.length === 0 ? (
            <TableRow>
              <TableCell colSpan={8} className="text-center text-gray-500 py-8">
                No accounts found
              </TableCell>
            </TableRow>
          ) : (
            accounts.map((account) => (
              <TableRow key={account.acctId}>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.acctId}`)}>
                    {account.acctId}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.acctId}`)}>
                    <span
                      className={`px-2 py-1 rounded text-xs font-semibold ${
                        account.isActive
                          ? 'bg-green-100 text-green-800'
                          : 'bg-gray-100 text-gray-800'
                      }`}
                    >
                      {account.activeStatusDisplayName}
                    </span>
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.acctId}`)}>
                    {formatCurrency(account.acctCurrBal)}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.acctId}`)}>
                    {formatCurrency(account.acctCreditLimit)}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.acctId}`)}>
                    <span
                      className={
                        account.availableCredit < 0 ? 'text-red-600 font-semibold' : ''
                      }
                    >
                      {formatCurrency(account.availableCredit)}
                    </span>
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.acctId}`)}>
                    {formatDate(account.acctOpenDate)}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/accounts/${account.acctId}`)}>
                    <span
                      className={`px-2 py-1 rounded text-xs font-semibold ${
                        account.isExpired
                          ? 'bg-red-100 text-red-800'
                          : 'bg-green-100 text-green-800'
                      }`}
                    >
                      {account.isExpired ? 'Yes' : 'No'}
                    </span>
                  </div>
                </TableCell>
                <TableCell>
                  <div className="flex gap-2">
                    <Button
                      size="sm"
                      onClick={(e) => {
                        e.stopPropagation();
                        router.push(`/accounts/${account.acctId}/edit`);
                      }}
                    >
                      Edit
                    </Button>
                    <Button
                      size="sm"
                      variant="danger"
                      onClick={(e) => {
                        e.stopPropagation();
                        handleDelete(account.acctId);
                      }}
                    >
                      Delete
                    </Button>
                  </div>
                </TableCell>
              </TableRow>
            ))
          )}
        </TableBody>
      </Table>

      {filterType === 'all' && totalPages > 1 && (
        <div className="mt-4 flex justify-between items-center">
          <Button
            variant="secondary"
            onClick={handlePreviousPage}
            disabled={currentPage === 0}
          >
            Previous
          </Button>
          <span className="text-sm text-gray-600">
            Page {currentPage + 1} of {totalPages}
          </span>
          <Button
            variant="secondary"
            onClick={handleNextPage}
            disabled={currentPage >= totalPages - 1}
          >
            Next
          </Button>
        </div>
      )}
    </div>
  );
}
