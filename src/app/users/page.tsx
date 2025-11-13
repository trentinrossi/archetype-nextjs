'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { userService } from '@/services/userService';
import { User } from '@/types/user';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell, Button, Input } from '@/components/ui';

export default function UsersPage() {
  const router = useRouter();
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchUserId, setSearchUserId] = useState('');
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [hasNext, setHasNext] = useState(false);
  const [currentDate, setCurrentDate] = useState('');
  const [currentTime, setCurrentTime] = useState('');

  const fetchUsers = useCallback(async (page: number = 0, search: string = '') => {
    try {
      setLoading(true);
      setError(null);
      
      let data;
      if (search.trim()) {
        data = await userService.searchUsers(search, page, 10);
      } else {
        data = await userService.getUsers(page, 10);
      }
      
      setUsers(data.content || []);
      setCurrentPage(data.pageable?.pageNumber || 0);
      setTotalPages(data.totalPages || 0);
      setHasNext(!data.last);
    } catch (err) {
      setError('Failed to load users');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    const user = userService.getCurrentUser();
    if (!user || user.userType !== 'A') {
      router.push('/login');
      return;
    }

    fetchUsers();
    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, [router, fetchUsers]);

  const updateDateTime = () => {
    const now = new Date();
    setCurrentDate(now.toLocaleDateString('en-US', { 
      month: '2-digit', 
      day: '2-digit', 
      year: 'numeric' 
    }));
    setCurrentTime(now.toLocaleTimeString('en-US', { 
      hour: '2-digit', 
      minute: '2-digit', 
      second: '2-digit',
      hour12: false 
    }));
  };

  const handleSearch = (e: React.FormEvent) => {
    e.preventDefault();
    fetchUsers(0, searchUserId);
  };

  const handlePreviousPage = () => {
    if (currentPage > 0) {
      fetchUsers(currentPage - 1, searchUserId);
    } else {
      setError('You are already at the top of the page...');
    }
  };

  const handleNextPage = () => {
    if (hasNext) {
      fetchUsers(currentPage + 1, searchUserId);
    } else {
      setError('You are already at the bottom of the page...');
    }
  };

  const handleDelete = async (id: number, userId: string) => {
    if (!confirm(`Are you sure you want to delete user ${userId}?`)) return;
    
    try {
      await userService.deleteUser(id);
      fetchUsers(currentPage, searchUserId);
    } catch (err) {
      alert('Failed to delete user');
      console.error(err);
    }
  };

  const handleBackToMenu = () => {
    router.push('/admin-menu');
  };

  if (loading && users.length === 0) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-lg">Loading...</div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 flex flex-col">
      <div className="bg-blue-900 text-white p-4">
        <div className="max-w-7xl mx-auto flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold">AWS Mainframe Modernization</h1>
            <h2 className="text-lg">CardDemo - User List</h2>
          </div>
          <div className="text-right">
            <div className="text-sm">Date: {currentDate}</div>
            <div className="text-sm">Time: {currentTime}</div>
          </div>
        </div>
      </div>

      <div className="flex-1 p-6">
        <div className="max-w-7xl mx-auto">
          <div className="bg-white shadow-lg rounded-lg p-6">
            <div className="mb-6">
              <div className="text-sm text-gray-600 mb-4">
                <span>Transaction: COUSR</span>
                <span className="ml-4">Program: COUSR00C</span>
              </div>
              <div className="flex justify-between items-center">
                <h3 className="text-xl font-bold text-gray-800">User List</h3>
                <Button onClick={() => router.push('/users/new')}>
                  Add New User
                </Button>
              </div>
            </div>

            {error && (
              <div className="mb-4 p-3 bg-red-50 border border-red-200 rounded text-red-700 text-sm">
                {error}
              </div>
            )}

            <form onSubmit={handleSearch} className="mb-6">
              <div className="flex gap-2">
                <Input
                  label="Search by User ID"
                  value={searchUserId}
                  onChange={(e) => setSearchUserId(e.target.value)}
                  placeholder="Enter User ID"
                  maxLength={8}
                />
                <div className="flex items-end gap-2">
                  <Button type="submit">
                    Search
                  </Button>
                  <Button 
                    type="button" 
                    variant="secondary"
                    onClick={() => {
                      setSearchUserId('');
                      fetchUsers(0, '');
                    }}
                  >
                    Clear
                  </Button>
                </div>
              </div>
            </form>

            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>User ID</TableHead>
                  <TableHead>First Name</TableHead>
                  <TableHead>Last Name</TableHead>
                  <TableHead>User Type</TableHead>
                  <TableHead>Status</TableHead>
                  <TableHead>Actions</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {users.length === 0 ? (
                  <TableRow>
                    <TableCell colSpan={6} className="text-center text-gray-500 py-8">
                      No users found
                    </TableCell>
                  </TableRow>
                ) : (
                  users.map((user) => (
                    <TableRow key={user.id}>
                      <TableCell>
                        <div className="cursor-pointer font-medium" onClick={() => router.push(`/users/${user.id}`)}>
                          {user.userId}
                        </div>
                      </TableCell>
                      <TableCell>
                        <div className="cursor-pointer" onClick={() => router.push(`/users/${user.id}`)}>
                          {user.firstName}
                        </div>
                      </TableCell>
                      <TableCell>
                        <div className="cursor-pointer" onClick={() => router.push(`/users/${user.id}`)}>
                          {user.lastName}
                        </div>
                      </TableCell>
                      <TableCell>
                        <div className="cursor-pointer" onClick={() => router.push(`/users/${user.id}`)}>
                          <span className={`px-2 py-1 rounded text-xs font-semibold ${
                            user.userType === 'A' ? 'bg-purple-100 text-purple-800' : 'bg-blue-100 text-blue-800'
                          }`}>
                            {user.userTypeDisplay}
                          </span>
                        </div>
                      </TableCell>
                      <TableCell>
                        <div className="cursor-pointer" onClick={() => router.push(`/users/${user.id}`)}>
                          <span className={`px-2 py-1 rounded text-xs font-semibold ${
                            user.authenticated ? 'bg-green-100 text-green-800' : 'bg-gray-100 text-gray-800'
                          }`}>
                            {user.authenticatedDisplay}
                          </span>
                        </div>
                      </TableCell>
                      <TableCell>
                        <div className="flex gap-2">
                          <Button 
                            size="sm" 
                            onClick={(e) => {
                              e.stopPropagation();
                              router.push(`/users/${user.id}/edit`);
                            }}
                          >
                            Edit
                          </Button>
                          <Button 
                            size="sm" 
                            variant="danger" 
                            onClick={(e) => {
                              e.stopPropagation();
                              handleDelete(user.id, user.userId);
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

            <div className="mt-6 flex justify-between items-center">
              <div className="text-sm text-gray-600">
                Page {currentPage + 1} of {totalPages || 1}
              </div>
              <div className="flex gap-2">
                <Button 
                  variant="secondary" 
                  onClick={handlePreviousPage}
                  disabled={currentPage === 0}
                >
                  Previous
                </Button>
                <Button 
                  variant="secondary" 
                  onClick={handleNextPage}
                  disabled={!hasNext}
                >
                  Next
                </Button>
                <Button 
                  variant="secondary" 
                  onClick={handleBackToMenu}
                >
                  Back to Menu
                </Button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
