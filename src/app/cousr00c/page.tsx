'use client';

import { useState, useEffect, useCallback } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { Button, Input, Table } from '@/components/ui';
import { userSecurityService } from '@/services/userSecurityService';
import { 
  UserSecurityDTO, 
  PaginatedResponse, 
  UserListParams,
  ApiError 
} from '@/types/userSecurity';

interface UserSelection {
  userId: string;
  action: 'U' | 'D' | '';
}

export default function UserListPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  
  const [users, setUsers] = useState<UserSecurityDTO[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchUserId, setSearchUserId] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(1);
  const [totalUsers, setTotalUsers] = useState(0);
  const [selections, setSelections] = useState<Record<string, UserSelection>>({});
  const [currentDateTime, setCurrentDateTime] = useState('');

  const pageSize = 10;

  // Update current date/time
  useEffect(() => {
    const updateDateTime = () => {
      const now = new Date();
      const formatted = now.toLocaleString('en-US', {
        year: 'numeric',
        month: '2-digit',
        day: '2-digit',
        hour: '2-digit',
        minute: '2-digit',
        second: '2-digit',
        hour12: false
      });
      setCurrentDateTime(formatted);
    };

    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, []);

  // Initialize page from URL params
  useEffect(() => {
    const page = searchParams.get('page');
    const search = searchParams.get('search');
    
    if (page) {
      setCurrentPage(parseInt(page, 10));
    }
    if (search) {
      setSearchUserId(search);
    }
  }, [searchParams]);

  const fetchUsers = useCallback(async (params?: UserListParams) => {
    setLoading(true);
    setError(null);

    try {
      const queryParams: UserListParams = {
        page: currentPage,
        limit: pageSize,
        search: searchUserId || undefined,
        sortBy: 'userId',
        sortOrder: 'asc',
        ...params
      };

      const response: PaginatedResponse<UserSecurityDTO> = await userSecurityService.getUsers(queryParams);
      
      setUsers(response.data);
      setCurrentPage(response.pagination.page);
      setTotalPages(response.pagination.totalPages);
      setTotalUsers(response.pagination.total);
      
      // Clear selections when data changes
      setSelections({});
      
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to fetch users';
      setError(errorMessage);
      setUsers([]);
    } finally {
      setLoading(false);
    }
  }, [currentPage, searchUserId]);

  // Fetch users on component mount and when dependencies change
  useEffect(() => {
    fetchUsers();
  }, [fetchUsers]);

  const handleSearch = () => {
    if (searchUserId.trim().length > 8) {
      setError('User ID cannot exceed 8 characters');
      return;
    }
    
    setCurrentPage(1);
    fetchUsers({ page: 1, search: searchUserId.trim() || undefined });
    
    // Update URL
    const params = new URLSearchParams();
    if (searchUserId.trim()) {
      params.set('search', searchUserId.trim());
    }
    params.set('page', '1');
    router.push(`/cousr00c?${params.toString()}`);
  };

  const handleClearSearch = () => {
    setSearchUserId('');
    setCurrentPage(1);
    fetchUsers({ page: 1, search: undefined });
    router.push('/cousr00c?page=1');
  };

  const handleSelectionChange = (userId: string, action: 'U' | 'D' | '') => {
    setSelections(prev => ({
      ...prev,
      [userId]: { userId, action }
    }));
    setError(null);
  };

  const handleProcessSelections = () => {
    const activeSelections = Object.values(selections).filter(sel => sel.action !== '');
    
    if (activeSelections.length === 0) {
      setError('Please select at least one user for update or delete');
      return;
    }

    if (activeSelections.length > 1) {
      setError('Please select only one user at a time');
      return;
    }

    const selection = activeSelections[0];
    const user = users.find(u => u.userId === selection.userId);
    
    if (!user) {
      setError('Selected user not found');
      return;
    }

    if (selection.action === 'U') {
      router.push(`/cousr02c?userId=${encodeURIComponent(user.userId)}&mode=update`);
    } else if (selection.action === 'D') {
      router.push(`/cousr03c?userId=${encodeURIComponent(user.userId)}&mode=delete`);
    }
  };

  const handlePF7PrevPage = () => {
    if (currentPage > 1) {
      const newPage = currentPage - 1;
      setCurrentPage(newPage);
      fetchUsers({ page: newPage });
      
      const params = new URLSearchParams();
      if (searchUserId.trim()) {
        params.set('search', searchUserId.trim());
      }
      params.set('page', newPage.toString());
      router.push(`/cousr00c?${params.toString()}`);
    }
  };

  const handlePF8NextPage = () => {
    if (currentPage < totalPages) {
      const newPage = currentPage + 1;
      setCurrentPage(newPage);
      fetchUsers({ page: newPage });
      
      const params = new URLSearchParams();
      if (searchUserId.trim()) {
        params.set('search', searchUserId.trim());
      }
      params.set('page', newPage.toString());
      router.push(`/cousr00c?${params.toString()}`);
    }
  };

  const handlePF3Exit = () => {
    router.push('/coadm01c');
  };

  const handleKeyPress = (e: React.KeyboardEvent) => {
    const key = e.key;
    
    if (key === 'F3') {
      e.preventDefault();
      handlePF3Exit();
      return;
    }
    
    if (key === 'F7') {
      e.preventDefault();
      handlePF7PrevPage();
      return;
    }
    
    if (key === 'F8') {
      e.preventDefault();
      handlePF8NextPage();
      return;
    }
    
    if (key === 'Enter') {
      e.preventDefault();
      if (document.activeElement?.id === 'searchUserId') {
        handleSearch();
      } else {
        handleProcessSelections();
      }
      return;
    }
  };

  const tableColumns = [
    {
      key: 'selection' as const,
      label: 'Sel',
      sortable: false,
      width: '80px',
      align: 'center' as const,
      render: (value: any, user: UserSecurityDTO) => (
        <select
          value={selections[user.userId]?.action || ''}
          onChange={(e) => handleSelectionChange(user.userId, e.target.value as 'U' | 'D' | '')}
          className="w-12 text-center border border-gray-300 rounded px-1 py-0.5 text-sm"
          disabled={loading}
        >
          <option value="">-</option>
          <option value="U">U</option>
          <option value="D">D</option>
        </select>
      )
    },
    {
      key: 'userId' as const,
      label: 'User ID',
      sortable: true,
      width: '120px',
      align: 'left' as const
    },
    {
      key: 'userType' as const,
      label: 'First Name',
      sortable: false,
      width: '150px',
      align: 'left' as const,
      render: (value: any, user: UserSecurityDTO) => user.userType || '-'
    },
    {
      key: 'programName' as const,
      label: 'Last Name',
      sortable: false,
      width: '150px',
      align: 'left' as const,
      render: (value: any, user: UserSecurityDTO) => user.programName || '-'
    },
    {
      key: 'userTypeDisplayName' as const,
      label: 'User Type',
      sortable: true,
      width: '120px',
      align: 'center' as const,
      render: (value: any, user: UserSecurityDTO) => user.userType
    }
  ];

  return (
    <div className="min-h-screen bg-gray-50" onKeyDown={handleKeyPress}>
      {/* Header */}
      <div className="bg-blue-600 text-white p-4">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-xl font-bold">COUSR00C - User List</h1>
            <p className="text-sm opacity-90">CardDemo - User Security Administration</p>
          </div>
          <div className="text-right text-sm">
            <p>Program: COUSR00C</p>
            <p>Date/Time: {currentDateTime}</p>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="p-6">
        {/* Search Section */}
        <div className="bg-white rounded-lg shadow-md p-6 mb-6">
          <div className="flex items-end gap-4">
            <div className="flex-1">
              <label htmlFor="searchUserId" className="block text-sm font-medium text-gray-700 mb-2">
                Search by User ID
              </label>
              <Input
                id="searchUserId"
                type="text"
                value={searchUserId}
                onChange={(e) => setSearchUserId(e.target.value.slice(0, 8).toUpperCase())}
                maxLength={8}
                placeholder="Enter User ID (max 8 chars)"
                disabled={loading}
                className="w-full"
              />
            </div>
            <div className="flex gap-2">
              <Button
                onClick={handleSearch}
                disabled={loading}
                className="bg-blue-600 hover:bg-blue-700 text-white"
              >
                Search
              </Button>
              <Button
                onClick={handleClearSearch}
                disabled={loading}
                variant="outline"
              >
                Clear
              </Button>
            </div>
          </div>
        </div>

        {/* Error Message */}
        {error && (
          <div className="mb-4 p-3 bg-red-100 border border-red-400 text-red-700 rounded">
            {error}
          </div>
        )}

        {/* User List Table */}
        <div className="bg-white rounded-lg shadow-md">
          <div className="p-4 border-b border-gray-200">
            <div className="flex justify-between items-center">
              <h2 className="text-lg font-semibold text-gray-900">
                User List ({totalUsers} users found)
              </h2>
              <div className="text-sm text-gray-600">
                Page {currentPage} of {totalPages}
              </div>
            </div>
          </div>

          {loading ? (
            <div className="p-8 text-center">
              <div className="inline-block animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
              <p className="mt-2 text-gray-600">Loading users...</p>
            </div>
          ) : users.length === 0 ? (
            <div className="p-8 text-center text-gray-600">
              {searchUserId ? 'No users found matching your search criteria.' : 'No users found.'}
            </div>
          ) : (
            <Table
              data={users}
              columns={tableColumns}
              className="w-full"
            />
          )}
        </div>

        {/* Pagination and Action Buttons */}
        <div className="mt-6 flex justify-between items-center">
          <div className="flex gap-2">
            <Button
              onClick={handlePF7PrevPage}
              disabled={loading || currentPage <= 1}
              variant="outline"
            >
              PF7 Prev Page
            </Button>
            <Button
              onClick={handlePF8NextPage}
              disabled={loading || currentPage >= totalPages}
              variant="outline"
            >
              PF8 Next Page
            </Button>
          </div>

          <div className="flex gap-2">
            <Button
              onClick={handleProcessSelections}
              disabled={loading || Object.values(selections).every(sel => sel.action === '')}
              className="bg-green-600 hover:bg-green-700 text-white"
            >
              Enter - Process Selection
            </Button>
            <Button
              onClick={handlePF3Exit}
              disabled={loading}
              variant="outline"
            >
              PF3 Exit to Admin Menu
            </Button>
          </div>
        </div>

        {/* Instructions */}
        <div className="mt-6 bg-gray-100 rounded-lg p-4">
          <h3 className="text-sm font-semibold text-gray-700 mb-2">Instructions:</h3>
          <div className="text-xs text-gray-600 space-y-1">
            <p>• Enter User ID in search field and press Enter or click Search to filter users</p>
            <p>• Select 'U' to update a user or 'D' to delete a user in the Sel column</p>
            <p>• Press Enter or click "Process Selection" to proceed with selected action</p>
            <p>• Use PF7/PF8 or click Prev/Next Page buttons to navigate between pages</p>
            <p>• Press F3 or click "Exit to Admin Menu" to return to the administration menu</p>
            <p>• Only one user can be selected at a time for update or delete operations</p>
          </div>
        </div>
      </div>
    </div>
  );
}