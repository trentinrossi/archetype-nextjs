'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/contexts/AuthContext';
import { userService } from '@/services/userService';
import { User } from '@/types/user';
import { Button, Input } from '@/components/ui';

export default function UserListPage() {
  const router = useRouter();
  const { isAuthenticated, isAdmin } = useAuth();
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [hasNext, setHasNext] = useState(false);
  const [hasPrevious, setHasPrevious] = useState(false);
  const [startUserId, setStartUserId] = useState('');
  const [selections, setSelections] = useState<Record<string, string>>({});
  const [currentDateTime, setCurrentDateTime] = useState('');

  useEffect(() => {
    if (!isAuthenticated || !isAdmin()) {
      router.push('/login');
      return;
    }
  }, [isAuthenticated, isAdmin, router]);

  useEffect(() => {
    const updateDateTime = () => {
      const now = new Date();
      const date = now.toLocaleDateString('en-US', { month: '2-digit', day: '2-digit', year: '2-digit' });
      const time = now.toLocaleTimeString('en-US', { hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false });
      setCurrentDateTime(`${date} ${time}`);
    };
    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, []);

  useEffect(() => {
    fetchUsers();
  }, [currentPage, startUserId]);

  const fetchUsers = async () => {
    try {
      setLoading(true);
      setError('');
      const response = await userService.getUsers(currentPage, 10, startUserId);
      setUsers(response.content);
      setTotalPages(response.totalPages);
      setHasNext(response.hasNext);
      setHasPrevious(response.hasPrevious);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load users');
    } finally {
      setLoading(false);
    }
  };

  const handleSelection = (userId: string, value: string) => {
    const upper = value.toUpperCase();
    if (upper === 'U' || upper === 'D' || upper === '') {
      setSelections({ ...selections, [userId]: upper });
      setError('');
    } else {
      setError('Invalid selection. Valid values are U and D');
    }
  };

  const handleProcess = (userId: string) => {
    const action = selections[userId];
    if (action === 'U') {
      router.push(`/admin/users/${userId}/edit`);
    } else if (action === 'D') {
      router.push(`/admin/users/${userId}/delete`);
    } else {
      setError('Please select an action (U or D)');
    }
  };

  const handlePrevPage = () => {
    if (hasPrevious) {
      setCurrentPage(currentPage - 1);
    } else {
      setError('You are already at the top of the page...');
    }
  };

  const handleNextPage = () => {
    if (hasNext) {
      setCurrentPage(currentPage + 1);
    } else {
      setError('You are already at the bottom of the page...');
    }
  };

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="bg-gradient-to-r from-blue-600 to-indigo-600 text-white shadow-lg">
        <div className="max-w-7xl mx-auto px-4 py-4">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold">CU00 - COUSR00C</h1>
              <p className="text-blue-100 text-sm">User List (Security)</p>
            </div>
            <p className="text-sm">{currentDateTime}</p>
          </div>
        </div>
      </div>

      <div className="max-w-7xl mx-auto px-4 py-6">
        <div className="bg-white rounded-lg shadow-lg p-6">
          <div className="mb-6 flex justify-between items-center">
            <div className="flex gap-4 items-end">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Filter by User ID
                </label>
                <Input
                  type="text"
                  value={startUserId}
                  onChange={(e) => setStartUserId(e.target.value)}
                  placeholder="Enter User ID"
                  className="w-48"
                />
              </div>
              <Button onClick={() => setCurrentPage(0)}>Search</Button>
            </div>
            <div className="text-sm text-gray-600">
              Page {currentPage + 1} of {totalPages || 1}
            </div>
          </div>

          {error && (
            <div className="mb-4 p-4 bg-red-50 border border-red-200 rounded-lg text-red-800">
              {error}
            </div>
          )}

          {loading ? (
            <div className="text-center py-12">
              <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
              <p className="mt-4 text-gray-600">Loading users...</p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Sel</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">User ID</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">First Name</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Last Name</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Type</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Action</th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {users.map((user) => (
                    <tr key={user.userId} className="hover:bg-gray-50">
                      <td className="px-6 py-4">
                        <Input
                          type="text"
                          value={selections[user.userId] || ''}
                          onChange={(e) => handleSelection(user.userId, e.target.value)}
                          className="w-12 text-center uppercase"
                          maxLength={1}
                        />
                      </td>
                      <td className="px-6 py-4 text-sm font-medium text-gray-900">{user.userId}</td>
                      <td className="px-6 py-4 text-sm text-gray-900">{user.firstName}</td>
                      <td className="px-6 py-4 text-sm text-gray-900">{user.lastName}</td>
                      <td className="px-6 py-4 text-sm">
                        <span className={`px-2 py-1 rounded-full text-xs font-semibold ${user.userType === 'A' ? 'bg-purple-100 text-purple-800' : 'bg-green-100 text-green-800'}`}>
                          {user.userType === 'A' ? 'Admin' : 'Regular'}
                        </span>
                      </td>
                      <td className="px-6 py-4">
                        <Button size="sm" onClick={() => handleProcess(user.userId)}>
                          Process
                        </Button>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}

          <div className="mt-6 flex justify-between items-center">
            <Button variant="secondary" onClick={() => router.push('/admin')}>
              PF3 - Return to Menu
            </Button>
            <div className="flex gap-2">
              <Button variant="secondary" onClick={handlePrevPage} disabled={!hasPrevious}>
                PF7 - Previous
              </Button>
              <Button variant="secondary" onClick={handleNextPage} disabled={!hasNext}>
                PF8 - Next
              </Button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
