'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { userService } from '@/services/userService';
import UserList from '@/components/UserList';
import { User } from '@/types/user';

export default function UserListPage() {
  const router = useRouter();
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [hasNext, setHasNext] = useState(false);
  const [hasPrevious, setHasPrevious] = useState(false);
  const [searchUserId, setSearchUserId] = useState('');
  const [currentDateTime, setCurrentDateTime] = useState(new Date());
  const [currentUser, setCurrentUser] = useState<any>(null);

  const pageSize = 10;

  useEffect(() => {
    const user = userService.getCurrentUser();
    if (!user || user.userType !== 'A') {
      router.push('/login');
      return;
    }
    setCurrentUser(user);

    const timer = setInterval(() => {
      setCurrentDateTime(new Date());
    }, 1000);

    return () => clearInterval(timer);
  }, [router]);

  useEffect(() => {
    if (currentUser) {
      fetchUsers();
    }
  }, [currentPage, currentUser]);

  const fetchUsers = async () => {
    try {
      setLoading(true);
      setError(null);

      const params: any = {
        page: currentPage,
        size: pageSize,
      };

      if (searchUserId.trim()) {
        params.startUserId = searchUserId.trim();
      }

      const response = await userService.getUserList(params);
      
      setUsers(response.content);
      setTotalPages(response.totalPages);
      setHasNext(response.hasNext);
      setHasPrevious(response.hasPrevious);
    } catch (err: any) {
      setError(err.message || 'Failed to load users');
      console.error('Error fetching users:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = (e: React.FormEvent) => {
    e.preventDefault();
    setCurrentPage(0);
    fetchUsers();
  };

  const handleClearSearch = () => {
    setSearchUserId('');
    setCurrentPage(0);
    fetchUsers();
  };

  const handlePageChange = (page: number) => {
    setCurrentPage(page);
  };

  const handleEdit = (user: User) => {
    router.push(`/admin/users/update?userId=${user.userId}`);
  };

  const handleDelete = async (userId: string) => {
    try {
      await userService.deleteUser(userId);
      fetchUsers();
    } catch (err: any) {
      alert(err.message || 'Failed to delete user');
    }
  };

  const handlePF3 = () => {
    router.push('/admin');
  };

  const formatDateTime = (date: Date): string => {
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const day = String(date.getDate()).padStart(2, '0');
    const year = date.getFullYear();
    const hours = String(date.getHours()).padStart(2, '0');
    const minutes = String(date.getMinutes()).padStart(2, '0');
    const seconds = String(date.getSeconds()).padStart(2, '0');
    return `${month}/${day}/${year} ${hours}:${minutes}:${seconds}`;
  };

  if (!currentUser) {
    return null;
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 flex flex-col">
      <header className="bg-white shadow-md">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4">
          <div className="flex justify-between items-center">
            <div className="flex items-center">
              <div className="flex items-center justify-center w-10 h-10 bg-indigo-600 rounded-lg mr-3">
                <svg className="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z" />
                </svg>
              </div>
              <div>
                <h1 className="text-2xl font-bold text-gray-900">User List</h1>
                <p className="text-xs text-gray-600">Transaction: CU00 | Program: COUSR00C</p>
              </div>
            </div>
            <div className="text-right">
              <div className="text-sm text-gray-600 font-medium">{formatDateTime(currentDateTime)}</div>
              <div className="text-xs text-gray-500 mt-1">User: {currentUser.firstName} {currentUser.lastName}</div>
            </div>
          </div>
        </div>
      </header>

      <main className="flex-1 px-4 sm:px-6 lg:px-8 py-8">
        <div className="max-w-7xl mx-auto">
          <div className="bg-white rounded-lg shadow-xl overflow-hidden">
            <div className="bg-gradient-to-r from-indigo-600 to-indigo-700 px-6 py-4">
              <h2 className="text-xl font-bold text-white">User Management</h2>
              <p className="text-indigo-100 text-sm mt-1">View and manage system users</p>
            </div>

            <div className="p-6">
              <form onSubmit={handleSearch} className="mb-6">
                <div className="flex gap-4 items-end">
                  <div className="flex-1 max-w-md">
                    <Input
                      label="Search by User ID"
                      value={searchUserId}
                      onChange={(e) => setSearchUserId(e.target.value)}
                      placeholder="Enter User ID"
                      maxLength={8}
                    />
                  </div>
                  <Button type="submit">Search</Button>
                  {searchUserId && (
                    <Button type="button" variant="secondary" onClick={handleClearSearch}>Clear</Button>
                  )}
                </div>
              </form>

              {error && (
                <div className="mb-6 bg-red-50 border border-red-200 rounded-lg p-4">
                  <p className="text-sm text-red-800">{error}</p>
                </div>
              )}

              <UserList
                users={users}
                loading={loading}
                currentPage={currentPage}
                totalPages={totalPages}
                hasNext={hasNext}
                hasPrevious={hasPrevious}
                onPageChange={handlePageChange}
                onEdit={handleEdit}
                onDelete={handleDelete}
              />
            </div>

            <div className="bg-gray-50 px-6 py-4 border-t border-gray-200">
              <div className="flex items-center justify-between">
                <div className="text-sm text-gray-600">
                  <span className="font-semibold">PF3</span> = Return to Admin Menu
                </div>
                <Button variant="secondary" onClick={handlePF3}>PF3 - Admin Menu</Button>
              </div>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
}
